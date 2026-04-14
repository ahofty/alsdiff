(** Path-to-NFA compiler and streaming evaluator.

    Declare path queries upfront, compile into a combined NFA with prefix
    sharing, evaluate all queries in a single pass over [Xml2] StAX events.
    No DOM allocation.

    Supports all [Upath] features: MultiWildcard ([**]), SingleWildcard ([*]),
    ParentNode ([..]), CurrentNode ([.]), Regex names, attribute constraints,
    and Index. *)

(* --- Types --- *)

type query_id = int

type name_matcher =
  | Exact of string
  | Regex of string * Re.Pcre.regexp  (* pattern string + compiled regexp *)
  | Any                                (* matches any element name *)

type attr_constraint = {
  name : string;
  value : string option;  (* None = existence check, Some v = exact match *)
}

type query = {
  qid : query_id;
  path : Upath.path_component list;  (** Parsed path from [Upath.parse_path] *)
  attr : string option;              (** [None] = match element, [Some name] = extract attribute *)
}

type match_result = {
  query_id : query_id;
  element_name : string;
  attrs : (string * string) list;
  depth : int;
}

(* --- NFA representation --- *)

type state_id = int

type transition = {
  tid : int;                    (* unique ID for fire-count tracking *)
  matcher : name_matcher;    (* label: how to match element names *)
  constraints : attr_constraint list;  (* attribute constraints *)
  target : state_id;
  index : int option;           (* None = normal, Some n = only fire at Nth match *)
  depth_limit : int option;     (* None = any depth, Some d = only at exact depth d *)
}

type nfa_state = {
  id : state_id;
  mutable transitions : transition list;
  mutable accepting : (query_id * attr_constraint list) list;
  mutable is_wildcard_loop : bool;      (* MultiWildcard self-propagation *)
  mutable end_transitions : state_id list;  (* ParentNode: activate on El_end *)
}

type nfa = {
  states : nfa_state array;
  start : state_id;
}

(* --- Bitmask active set --- *)
type active_set = int

let empty_active = 0

let add_active sid bits = bits lor (1 lsl sid)

let mem_active sid bits = (bits land (1 lsl sid)) <> 0

let single_active sid = 1 lsl sid

let iter_active f bits =
  let b = ref bits in
  while !b <> 0 do
    let lowest = !b land (- !b) in
    f (Ocaml_intrinsics_kernel.Int.count_trailing_zeros lowest);
    b := !b - lowest
  done

(* Evaluator stack frame — needed for ParentNode name/attr resolution *)
type stack_frame = {
  element_name : string;
  element_attrs : (string * string) list;
  active : active_set;
}

(* --- Name / attribute matching --- *)

let match_name tag_name = function
  | Exact s -> tag_name = s
  | Regex (_, re) -> Re.execp re tag_name
  | Any -> true

let check_attrs (xml_attrs : (string * string) list) (constraints : attr_constraint list) =
  List.for_all (fun (c : attr_constraint) ->
      match List.assoc_opt c.name xml_attrs with
      | None -> false
      | Some v ->
        match c.value with
        | None -> true          (* existence check *)
        | Some expected -> v = expected
    ) constraints

(* --- Name matcher equality (for prefix sharing) --- *)

let equal_name_matcher a b =
  match a, b with
  | Exact sa, Exact sb -> sa = sb
  | Regex (pa, _), Regex (pb, _) -> pa = pb
  | Any, Any -> true
  | _ -> false

let equal_attr_constraints (a : attr_constraint list) (b : attr_constraint list) =
  List.length a = List.length b &&
  List.for_all2 (fun (x : attr_constraint) (y : attr_constraint) ->
      x.name = y.name &&
      match x.value, y.value with
      | None, None -> true
      | Some vx, Some vy -> vx = vy
      | _ -> false
    ) a b

let equal_transition_key t matcher constraints index depth_limit =
  equal_name_matcher t.matcher matcher && equal_attr_constraints t.constraints constraints &&
  t.index = index && t.depth_limit = depth_limit

(* --- Upath -> upath2 type conversion --- *)

let convert_name_component = function
  | Upath.Raw s -> Exact s
  | Upath.Regex (p, re) -> Regex (p, re)

let convert_attributes (attrs : Upath.attribute list) : attr_constraint list =
  List.map (fun (a : Upath.attribute) ->
      { name = a.name;
        value = match a.value with
          | Upath.Exact v -> Some v
          | Upath.Any -> None }
    ) attrs

(* --- Compilation --- *)

let compile (queries : query list) =
  let states = Hashtbl.create 32 in
  let next_id = ref 0 in
  let next_tid = ref 0 in
  let make_state () =
    let id = !next_id in
    incr next_id;
    let s = { id; transitions = []; accepting = [];
              is_wildcard_loop = false; end_transitions = [] } in
    Hashtbl.add states id s;
    s
  in
  let start_state = make_state () in
  (* Root-fallback: start state self-propagates so that paths not starting
     with the root element name (e.g. ["/Tracks/*/Name/EffectiveName"]) are
     still tried at every depth, matching DOM-based Upath behavior. *)
  start_state.is_wildcard_loop <- true;
  (* Build NFA with prefix sharing *)
  List.iter (fun q ->
      let rec walk path (state : nfa_state) =
        match path with
        | [] ->
          state.accepting <- (q.qid, []) :: state.accepting
        | comp :: rest ->
          match comp with
          | Upath.Tag (name_comp, attrs) ->
            let matcher = convert_name_component name_comp in
            let cattrs = convert_attributes attrs in
            (match List.find_opt (fun t -> equal_transition_key t matcher cattrs None None) state.transitions with
             | Some t ->
               walk rest (Hashtbl.find states t.target)
             | None ->
               let new_state = make_state () in
               let tid = !next_tid in
               incr next_tid;
               state.transitions <- { tid; matcher; constraints = cattrs; target = new_state.id; index = None; depth_limit = None } :: state.transitions;
               walk rest new_state)
          | Upath.SingleWildcard attrs ->
            let cattrs = convert_attributes attrs in
            let depth_limit = if state.id = start_state.id then Some 2 else None in
            (match List.find_opt (fun t -> equal_transition_key t Any cattrs None depth_limit) state.transitions with
             | Some t ->
               walk rest (Hashtbl.find states t.target)
             | None ->
               let new_state = make_state () in
               let tid = !next_tid in
               incr next_tid;
               state.transitions <- { tid; matcher = Any; constraints = cattrs; target = new_state.id; index = None; depth_limit } :: state.transitions;
               walk rest new_state)
          | Upath.MultiWildcard attrs ->
            let cattrs = convert_attributes attrs in
            (* Find or create a wildcard-loop state *)
            let wl_state =
              match List.find_opt (fun t ->
                  t.matcher = Any && equal_attr_constraints t.constraints cattrs &&
                  t.index = None && t.depth_limit = None
                ) state.transitions with
              | Some t -> Hashtbl.find states t.target
              | None ->
                let new_state = make_state () in
                new_state.is_wildcard_loop <- true;
                let tid = !next_tid in
                incr next_tid;
                state.transitions <- { tid; matcher = Any; constraints = cattrs; target = new_state.id; index = None; depth_limit = None } :: state.transitions;
                new_state
            in
            if rest = [] then begin
              (* MultiWildcard is last component: add accepting with attrs for self-match *)
              wl_state.accepting <- (q.qid, cattrs) :: wl_state.accepting
            end else begin
              (* Continue compiling rest from the wildcard state *)
              walk rest wl_state
            end
          | Upath.ParentNode ->
            (* Add end_transition: target state activates when El_end fires *)
            let target_state = make_state () in
            state.end_transitions <- target_state.id :: state.end_transitions;
            walk rest target_state
          | Upath.CurrentNode ->
            (* Epsilon: skip to next component in same state *)
            walk rest state
          | Upath.Index (n, name_comp_opt) ->
            let matcher = match name_comp_opt with
              | Some nc -> convert_name_component nc
              | None -> Any
            in
            let depth_limit = if state.id = start_state.id then Some 2 else None in
            (match List.find_opt (fun t -> equal_transition_key t matcher [] (Some n) depth_limit) state.transitions with
             | Some t ->
               walk rest (Hashtbl.find states t.target)
             | None ->
               let new_state = make_state () in
               let tid = !next_tid in
               incr next_tid;
               state.transitions <- { tid; matcher; constraints = []; target = new_state.id; index = Some n; depth_limit } :: state.transitions;
               walk rest new_state)
      in
      walk q.path start_state
    ) queries;
  (* Build array from hashtable *)
  let state_count = !next_id in
  assert (state_count <= Sys.int_size - 1);
  let state_arr = Array.make state_count (Obj.magic () : nfa_state) in
  Hashtbl.iter (fun id s -> state_arr.(id) <- s) states;
  { states = state_arr; start = start_state.id }

(* --- Evaluation --- *)

let evaluate nfa stream =
  let results = ref [] in
  let fire_counts : (int, int) Hashtbl.t = Hashtbl.create 16 in
  let stack = ref [{
      element_name = "";
      element_attrs = [];
      active = single_active nfa.start;
    }] in
  Xml2.iter_signals (fun sigv ->
      match sigv with
      | Xml2.El_start (name, attrs) ->
        let frame = List.hd !stack in
        let active = frame.active in
        let new_active = ref empty_active in
        let add_state sid =
          new_active := !new_active lor (1 lsl sid)
        in
        iter_active (fun sid ->
            let state = nfa.states.(sid) in
            (* Wildcard loop: self-propagate *)
            if state.is_wildcard_loop then
              add_state sid;
            (* Normal transitions *)
            List.iter (fun t ->
                if match_name name t.matcher && check_attrs attrs t.constraints then begin
                  let depth_ok = match t.depth_limit with
                    | None -> true
                    | Some d -> Xml2.depth stream = d
                  in
                  if depth_ok then begin
                    let fire = match t.index with
                      | None -> true
                      | Some n ->
                        let count = Hashtbl.find_opt fire_counts t.tid
                          |> Option.value ~default:0 in
                        Hashtbl.replace fire_counts t.tid (count + 1);
                        count = n
                    in
                    if fire then begin
                      add_state t.target;
                      (* Check accepting at target *)
                      let target_state = nfa.states.(t.target) in
                      List.iter (fun (qid, acc_attrs) ->
                          if check_attrs attrs acc_attrs then
                            results := { query_id = qid; element_name = name;
                                         attrs; depth = Xml2.depth stream } :: !results
                        ) target_state.accepting
                    end
                  end
                end
              ) state.transitions;
            (* Wildcard self-match accepting *)
            if state.is_wildcard_loop then
              List.iter (fun (qid, acc_attrs) ->
                  if check_attrs attrs acc_attrs then
                    results := { query_id = qid; element_name = name;
                                 attrs; depth = Xml2.depth stream } :: !results
                ) state.accepting
          ) active;
        stack := { element_name = name; element_attrs = attrs; active = !new_active } :: !stack
      | Xml2.El_end ->
        (match !stack with
         | popped :: parent :: rest ->
           (* End transitions: ParentNode handling *)
           let end_targets = ref empty_active in
           iter_active (fun sid ->
               let state = nfa.states.(sid) in
               List.iter (fun target_id ->
                   end_targets := !end_targets lor (1 lsl target_id)
                 ) state.end_transitions
             ) popped.active;
           (* Merge end targets into parent active set *)
           let merged_active = parent.active lor !end_targets in
           iter_active (fun target_id ->
               (* Check accepting at end-transition target *)
               let target_state = nfa.states.(target_id) in
               List.iter (fun (qid, acc_attrs) ->
                   if check_attrs popped.element_attrs acc_attrs then
                     results := { query_id = qid; element_name = popped.element_name;
                                  attrs = popped.element_attrs;
                                  depth = Xml2.depth stream } :: !results
                 ) target_state.accepting
             ) !end_targets;
           stack := { parent with active = merged_active } :: rest
         | popped :: rest ->
           (* Top-level: just pop *)
           ignore popped;
           stack := rest
         | [] -> ())
      | Data _ -> ()
    ) stream;
  List.rev !results

(* --- Convenience API --- *)

(** Backward-compatible constructor from string list *)
let simple_query ~qid ~path ~attr =
  { qid; path = List.map (fun s -> Upath.Tag (Upath.Raw s, [])) path; attr }

(** Constructor from parsed path *)
let query_of_path ~qid ~path_str ~attr =
  { qid; path = Upath.parse_path path_str; attr }

(* --- Helpers --- *)

let get_attr result name =
  List.assoc_opt name result.attrs

let get_int_attr result name =
  Option.bind (get_attr result name) int_of_string_opt

let get_float_attr result name =
  Option.bind (get_attr result name) float_of_string_opt

(* --- Pretty printers --- *)

let pp_name_matcher fmt = function
  | Exact s -> Fmt.pf fmt "%s" s
  | Regex (p, _) -> Fmt.pf fmt "'%s'" p
  | Any -> Fmt.pf fmt "*"

let pp_attr_constraint fmt (c : attr_constraint) =
  match c.value with
  | None -> Fmt.pf fmt "@%s" c.name
  | Some v -> Fmt.pf fmt "@%s=\"%s\"" c.name v

let pp_transition fmt t =
  let index_str = match t.index with
    | None -> "" | Some n -> Printf.sprintf "[%d]" n
  in
  let depth_str = match t.depth_limit with
    | None -> "" | Some d -> Printf.sprintf " depth=%d" d
  in
  Fmt.pf fmt "  -[%a%a%s%s]-> %d"
    pp_name_matcher t.matcher
    (Fmt.list pp_attr_constraint) t.constraints
    index_str depth_str
    t.target

let pp_nfa_state fmt s =
  Fmt.pf fmt "State %d%s%s%s@\n%a"
    s.id
    (match s.accepting with [] -> "" | qs -> Printf.sprintf " [accept: %s]"
                                               (String.concat "," (List.map (fun (qid, _) -> string_of_int qid) qs)))
    (if s.is_wildcard_loop then " [wildcard-loop]" else "")
    (match s.end_transitions with [] -> "" | targets -> Printf.sprintf " [end->%s]"
                                                          (String.concat "," (List.map string_of_int targets)))
    (Fmt.list pp_transition) s.transitions

let pp_nfa fmt nfa =
  Fmt.pf fmt "NFA: %d states, start=%d@\n%a"
    (Array.length nfa.states) nfa.start
    (Fmt.list pp_nfa_state) (Array.to_list nfa.states)

let pp_match_result fmt r =
  let attr_str = match get_attr r "Value" with
    | Some v -> Printf.sprintf " Value=%s" v
    | None -> (match r.attrs with
          [] -> ""
        | _ -> Printf.sprintf " attrs=%s"
                 (String.concat "," (List.map (fun (k, v) -> k ^ "=" ^ v) r.attrs)))
  in
  Fmt.pf fmt "q%d: <%s> at depth %d%s" r.query_id r.element_name r.depth attr_str
