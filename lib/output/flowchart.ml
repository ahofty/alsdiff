open Alsdiff_base
open Alsdiff_live

module IntMap = Map.Make (Int)
module IntSet = Set.Make (Int)

type edge_style =
  | Routing
  | Send
  | InputRouting

type node = {
  id : string;
  label : string;
}

type edge = {
  from_id : string;
  to_id : string;
  label : string;
  style : edge_style;
}

type track_info = {
  node : node;
  group_label : string option;
}

type group_info = {
  track_parent : int option IntMap.t;
  group_ids : IntSet.t;
  track_order : int list;
}

type options = {
  direction : string;
  include_external : bool;
  include_routing : bool;
  include_sends : bool;
  use_subgraph_id_for_groups : bool;
}

let sanitize_id (s : string) : string =
  let buf = Buffer.create (String.length s) in
  String.iter (fun ch ->
      match ch with
      | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_' -> Buffer.add_char buf ch
      | _ -> Buffer.add_char buf '_'
    ) s;
  let out = Buffer.contents buf in
  if out = "" then "n"
  else
    match out.[0] with
    | 'a' .. 'z' | 'A' .. 'Z' -> out
    | _ -> "n_" ^ out

(* Basic label sanitizer for internal use (used by track_node_info).
   Each renderer has its own additional escaping for output. *)
let sanitize_label (s : string) : string =
  let s = String.map (fun ch -> if ch = '\n' || ch = '\r' then ' ' else ch) s in
  let s = String.concat "\\\"" (String.split_on_char '"' s) in
  s

let collapse_whitespace (s : string) : string =
  let buf = Buffer.create (String.length s) in
  let is_space = function ' ' | '\t' | '\n' | '\r' -> true | _ -> false in
  let rec loop i in_space =
    if i >= String.length s then ()
    else
      let ch = s.[i] in
      if is_space ch then
        if in_space then loop (i + 1) true
        else (Buffer.add_char buf ' '; loop (i + 1) true)
      else (Buffer.add_char buf ch; loop (i + 1) false)
  in
  loop 0 false;
  Buffer.contents buf |> String.trim

let normalize_external_label (s : string) : string * string =
  let collapsed = collapse_whitespace s in
  let normalized = String.lowercase_ascii collapsed in
  let key =
    match normalized with
    | "" | "none" | "no output" | "midiout/none" | "audioout/none" -> "no output"
    | _ -> normalized
  in
  let display =
    match key with
    | "no output" -> "No Output"
    | _ -> key
  in
  (display, key)

let format_param_value (value : Device.param_value) : string =
  match value with
  | Device.Float f -> Printf.sprintf "%.2f" f
  | Device.Int i -> string_of_int i
  | Device.Bool b -> if b then "on" else "off"
  | Device.Enum (i, _) -> string_of_int i

let format_generic_param (p : Device.GenericParam.t) : string =
  format_param_value p.value

let mixer_label (m : Track.Mixer.t) : string =
  let vol = format_generic_param m.volume in
  let pan = format_generic_param m.pan in
  let mute = format_generic_param m.mute in
  let solo = format_generic_param m.solo in
  Printf.sprintf "vol=%s pan=%s mute=%s solo=%s" vol pan mute solo

let send_label (s : Track.Send.t) : string =
  let amt = format_generic_param s.amount in
  Printf.sprintf "send=%s" amt

let is_no_output (s : string) : bool =
  let s = collapse_whitespace s |> String.lowercase_ascii in
  s = "" || s = "none" || s = "no output" || s = "midiout/none" || s = "audioout/none"

let parse_trailing_int (s : string) : int option =
  let len = String.length s in
  let rec find_start i =
    if i < 0 then None
    else
      match s.[i] with
      | '0' .. '9' ->
        let rec scan j =
          if j >= 0 then
            match s.[j] with
            | '0' .. '9' -> scan (j - 1)
            | _ -> Some (j + 1)
          else Some 0
        in
        scan i
      | _ -> find_start (i - 1)
  in
  match find_start (len - 1) with
  | None -> None
  | Some start ->
    let substr = String.sub s start (len - start) in
    int_of_string_opt substr

let parse_track_id_from_target (target : string) : int option =
  let len = String.length target in
  let marker = "Track." in
  let marker_len = String.length marker in
  let is_digit ch = ch >= '0' && ch <= '9' in
  let rec find i =
    if i + marker_len > len then None
    else if String.sub target i marker_len = marker then
      let start = i + marker_len in
      if start >= len || not (is_digit target.[start]) then find (i + 1)
      else
        let rec consume j =
          if j < len && is_digit target.[j] then consume (j + 1)
          else j
        in
        let stop = consume start in
        let digits = String.sub target start (stop - start) in
        int_of_string_opt digits
    else find (i + 1)
  in
  find 0

let parse_target_track_id (target : string) : int option =
  match int_of_string_opt (String.trim target) with
  | Some n -> Some n
  | None ->
    (match parse_track_id_from_target target with
     | Some n -> Some n
     | None ->
       if String.contains target '/' || String.contains target '.'
       then None
       else parse_trailing_int target)

let routing_label_target (r : Track.Routing.t) : string =
  if not (is_no_output r.upper_string) then r.upper_string
  else if not (is_no_output r.lower_string) then r.lower_string
  else r.target

let is_group_keyword (s : string) : bool =
  String.lowercase_ascii (String.trim s) = "group"

let is_group_routing_target (r : Track.Routing.t) : bool =
  is_group_keyword r.target || is_group_keyword (routing_label_target r)

let is_main_keyword (s : string) : bool =
  String.lowercase_ascii (String.trim s) = "main"

let group_subgraph_id (group_id : int) : string =
  sanitize_id ("group_" ^ string_of_int group_id)

let should_skip_routing (r : Track.Routing.t) : bool =
  is_no_output r.target && is_no_output (routing_label_target r)

(* render_edge is now renderer-specific:
   - Mermaid_renderer.render_edge: uses `-->` and `-.->`
   - Dot_renderer.render_edge: uses `->` and `[style=dashed]` *)

let track_node_info (track : Track.t) : track_info option * int option =
  match track with
  | Track.Midi t ->
    let node = { id = "track_" ^ string_of_int t.id; label = sanitize_label (t.name ^ " (MIDI)") } in
    (Some { node; group_label = None }, Some t.id)
  | Track.Audio t ->
    let node = { id = "track_" ^ string_of_int t.id; label = sanitize_label (t.name ^ " (Audio)") } in
    (Some { node; group_label = None }, Some t.id)
  | Track.Group t ->
    let node = { id = "track_" ^ string_of_int t.id; label = sanitize_label (t.name ^ " (Group)") } in
    (Some { node; group_label = Some t.name }, Some t.id)
  | Track.Return t ->
    let node = { id = "track_" ^ string_of_int t.id; label = sanitize_label (t.name ^ " (Return)") } in
    (Some { node; group_label = None }, Some t.id)
  | Track.Main _ -> (None, None)

let routing_for_track (track : Track.t) : Track.Routing.t option =
  match track with
  | Track.Midi t -> Some t.routings.midi_out
  | Track.Audio t | Track.Group t | Track.Return t -> Some t.routings.audio_out
  | Track.Main _ -> None

let input_routing_for_track (track : Track.t) : Track.Routing.t option =
  match track with
  | Track.Midi t -> Some t.routings.midi_in
  | Track.Audio t | Track.Group t | Track.Return t -> Some t.routings.audio_in
  | Track.Main _ -> None

let mixer_for_track (track : Track.t) : Track.Mixer.t option =
  match track with
  | Track.Midi t -> Some t.mixer
  | Track.Audio t | Track.Group t | Track.Return t -> Some t.mixer
  | Track.Main _ -> None

let sends_for_track (track : Track.t) : Track.Send.t list =
  match track with
  | Track.Midi t -> t.mixer.sends
  | Track.Audio t | Track.Group t | Track.Return t -> t.mixer.sends
  | Track.Main _ -> []


let build_group_info (xml : Xml.t) ~(track_order : int list) : group_info =
  let liveset_xml = Upath.find "LiveSet" xml |> snd in
  let tracks_xml = Upath.find "Tracks" liveset_xml |> snd in
  let track_parent = ref IntMap.empty in
  let group_ids = ref IntSet.empty in
  let add_track xml =
    let id = Xml.get_int_attr "Id" xml in
    let parent =
      match Upath.get_int_attr "/TrackGroupId" "Value" xml with
      | -1 -> None
      | x -> Some x
    in
    track_parent := IntMap.add id parent !track_parent;
    match Xml.get_name xml with
    | "GroupTrack" -> group_ids := IntSet.add id !group_ids
    | _ -> ()
  in
  tracks_xml
  |> Xml.get_childs
  |> List.iter (fun track_xml ->
      match Xml.get_name track_xml with
      | "MidiTrack" | "AudioTrack" | "GroupTrack" | "ReturnTrack" -> add_track track_xml
      | _ -> ()
    );
  { track_parent = !track_parent; group_ids = !group_ids; track_order }

let resolve_group_parent_node_id
    ~(source_track_id : int option)
    ~(track_parent : int option IntMap.t)
    ~(group_ids : IntSet.t)
    ~(track_id_map : string IntMap.t)
  : string option =
  match source_track_id with
  | Some source_id ->
    (match IntMap.find_opt source_id track_parent with
     | Some (Some parent_id) when IntSet.mem parent_id group_ids ->
       IntMap.find_opt parent_id track_id_map
     | _ -> None)
  | None -> None

let should_suppress_parent_group_routing_edge
    ~(source_track_id : int option)
    ~(target_id : string)
    ~(track_parent : int option IntMap.t)
    ~(group_ids : IntSet.t)
    ~(track_id_map : string IntMap.t)
  : bool =
  match resolve_group_parent_node_id ~source_track_id ~track_parent ~group_ids ~track_id_map with
  | Some parent_node_id -> parent_node_id = target_id
  | None -> false

let input_routing_label (r : Track.Routing.t) : string =
  let lower = String.trim r.lower_string in
  if lower <> "" && not (is_no_output lower) then lower
  else
    let upper = String.trim r.upper_string in
    if upper <> "" && not (is_no_output upper) then upper
    else ""

let resolve_track_node_id_from_target ~(target : string) ~(track_id_map : string IntMap.t) : string option =
  match parse_target_track_id target with
  | Some id -> IntMap.find_opt id track_id_map
  | None -> None

let routing_from_id_for_main_target
    ~(source_track_id : int option)
    ~(default_from_id : string)
    ~(target_id : string)
    ~(group_ids : IntSet.t)
    ~(main_node_id : string)
    ~(use_subgraph_id : bool)
  : string =
  if target_id = main_node_id && use_subgraph_id then
    match source_track_id with
    | Some source_id when IntSet.mem source_id group_ids -> group_subgraph_id source_id
    | _ -> default_from_id
  else default_from_id

let build_graph ~(xml : Xml.t) ~(liveset : Liveset.t) ~(options : options)
  : track_info IntMap.t * node * node list * edge list * group_info =
  let track_info_map = ref IntMap.empty in
  let add_track track =
    let info_opt, id_opt = track_node_info track in
    match info_opt, id_opt with
    | Some info, Some id -> track_info_map := IntMap.add id info !track_info_map
    | _ -> ()
  in
  List.iter add_track liveset.tracks;
  List.iter add_track liveset.returns;

  let main_node = { id = "main"; label = "Main" } in

  let track_id_map =
    IntMap.fold (fun id info acc -> IntMap.add id info.node.id acc) !track_info_map IntMap.empty
  in
  let track_order =
    let ids = ref [] in
    let add_id track =
      match track_node_info track with
      | _, Some id -> ids := id :: !ids
      | _ -> ()
    in
    List.iter add_id liveset.tracks;
    List.iter add_id liveset.returns;
    List.rev !ids
  in
  let group_info = build_group_info xml ~track_order in
  let return_id_set =
    List.fold_left (fun acc track ->
        match track with
        | Track.Return t -> IntSet.add t.id acc
        | _ -> acc
      ) IntSet.empty liveset.returns
  in

  let external_nodes = Hashtbl.create 32 in
  let external_list = ref [] in

  let get_external_node_id ~label =
    let display_label, key = normalize_external_label label in
    match Hashtbl.find_opt external_nodes key with
    | Some id -> id
    | None ->
      if not options.include_external then "external"
      else
        let raw_id = "ext_" ^ string_of_int (abs (Hashtbl.hash key)) in
        let id = sanitize_id raw_id in
        Hashtbl.add external_nodes key id;
        external_list := { id; label = sanitize_label display_label } :: !external_list;
        id
  in

  let resolve_routing_target ~(source_track_id : int option) (r : Track.Routing.t) : string option =
    let fallback_external () =
      if options.include_external then
        let label = routing_label_target r in
        Some (get_external_node_id ~label)
      else None
    in
    match parse_target_track_id r.target with
    | Some id ->
      (match IntMap.find_opt id track_id_map with
       | Some node_id -> Some node_id
       | None -> fallback_external ())
    | None ->
      if is_group_routing_target r then
        match resolve_group_parent_node_id
                ~source_track_id
                ~track_parent:group_info.track_parent
                ~group_ids:group_info.group_ids
                ~track_id_map
        with
        | Some node_id -> Some node_id
        | None -> fallback_external ()
      else if is_main_keyword r.target || is_main_keyword (routing_label_target r) then
        Some main_node.id
      else fallback_external ()
  in

  let resolve_input_source_node_id (r : Track.Routing.t) : string option =
    match resolve_track_node_id_from_target ~target:r.target ~track_id_map with
    | Some id -> Some id
    | None when options.include_external ->
      let label =
        match String.trim (input_routing_label r) with
        | "" -> routing_label_target r
        | s -> s
      in
      Some (get_external_node_id ~label)
    | None -> None
  in

  let edges = ref [] in

  let add_routing_edge track =
    match routing_for_track track, mixer_for_track track with
    | Some routing, Some mixer when options.include_routing && not (should_skip_routing routing) ->
      let from_id, source_track_id =
        match track_node_info track with
        | Some info, Some id -> (info.node.id, Some id)
        | _ -> (main_node.id, None)
      in
      (match resolve_routing_target ~source_track_id routing with
       | Some target_id ->
         if should_suppress_parent_group_routing_edge
             ~source_track_id
             ~target_id
             ~track_parent:group_info.track_parent
             ~group_ids:group_info.group_ids
             ~track_id_map
         then ()
         else
           let from_id = routing_from_id_for_main_target
               ~source_track_id
               ~default_from_id:from_id
               ~target_id
               ~group_ids:group_info.group_ids
               ~main_node_id:main_node.id
               ~use_subgraph_id:options.use_subgraph_id_for_groups
           in
           let label = mixer_label mixer |> sanitize_label in
           edges := { from_id; to_id = target_id; label; style = Routing } :: !edges
       | None -> ())
    | _ -> ()
  in

  let add_input_routing_edge track =
    match input_routing_for_track track with
    | Some routing when options.include_routing && not (should_skip_routing routing) ->
      (match track_node_info track with
       | Some info, _ ->
         (match resolve_input_source_node_id routing with
          | Some source_id when source_id <> info.node.id ->
            let label = input_routing_label routing |> sanitize_label in
            edges := { from_id = source_id; to_id = info.node.id; label; style = InputRouting } :: !edges
          | _ -> ())
       | _ -> ())
    | _ -> ()
  in

  let add_send_edges track =
    if options.include_sends then
      let from_id =
        match track_node_info track with
        | Some info, _ -> info.node.id
        | _ -> main_node.id
      in
      let sends = sends_for_track track in
      List.iter (fun (send : Track.Send.t) ->
          let target_id =
            match IntMap.find_opt send.id track_id_map with
            | Some id -> id
            | None ->
              if options.include_external then
                let label = Printf.sprintf "Send Target %d" send.id in
                get_external_node_id ~label
              else "external"
          in
          if IntSet.mem send.id return_id_set || options.include_external then
            let label = send_label send |> sanitize_label in
            edges := { from_id; to_id = target_id; label; style = Send } :: !edges
        ) sends
  in

  List.iter (fun t -> add_routing_edge t; add_input_routing_edge t; add_send_edges t) liveset.tracks;
  List.iter (fun t -> add_routing_edge t; add_input_routing_edge t; add_send_edges t) liveset.returns;
  add_routing_edge liveset.main;
  (!track_info_map, main_node, List.rev !external_list, List.rev !edges, group_info)

(* Rendering is now renderer-specific:
   - Mermaid_renderer.render_flowchart: generates Mermaid flowchart syntax
   - Dot_renderer.render_flowchart: generates Graphviz DOT syntax *)
