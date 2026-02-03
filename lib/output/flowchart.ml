open Alsdiff_base
open Alsdiff_live

module IntMap = Map.Make (Int)
module IntSet = Set.Make (Int)

type edge_style =
  | Routing
  | Send

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

let sanitize_label (s : string) : string =
  let s = String.map (fun ch -> if ch = '\n' || ch = '\r' then ' ' else ch) s in
  let s = String.concat "/" (String.split_on_char '|' s) in
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
    | "" | "none" | "no output" -> "no output"
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
  let s = String.trim s |> String.lowercase_ascii in
  s = "" || s = "no output"

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

let parse_target_track_id (target : string) : int option =
  match int_of_string_opt (String.trim target) with
  | Some n -> Some n
  | None -> parse_trailing_int target

let routing_label_target (r : Track.Routing.t) : string =
  if not (is_no_output r.upper_string) then r.upper_string
  else if not (is_no_output r.lower_string) then r.lower_string
  else r.target

let should_skip_routing (r : Track.Routing.t) : bool =
  String.trim r.target = "" && is_no_output r.upper_string && is_no_output r.lower_string

let render_edge (e : edge) : string =
  let arrow = match e.style with Routing -> "-->" | Send -> "-.->" in
  if String.trim e.label = "" then
    Printf.sprintf "%s %s %s" e.from_id arrow e.to_id
  else
    Printf.sprintf "%s %s|%s| %s" e.from_id arrow e.label e.to_id

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

  let resolve_routing_target (r : Track.Routing.t) : string option =
    match parse_target_track_id r.target with
    | Some id ->
      (match IntMap.find_opt id track_id_map with
       | Some node_id -> Some node_id
       | None ->
         if options.include_external then
           let label = routing_label_target r in
           Some (get_external_node_id ~label)
         else None)
    | None ->
      if options.include_external then
        let label = routing_label_target r in
        Some (get_external_node_id ~label)
      else None
  in

  let edges = ref [] in

  let add_routing_edge track =
    match routing_for_track track, mixer_for_track track with
    | Some routing, Some mixer when options.include_routing && not (should_skip_routing routing) ->
      (match resolve_routing_target routing with
       | Some target_id ->
         let from_id =
           match track_node_info track with
           | Some info, _ -> info.node.id
           | _ -> main_node.id
         in
         let label = mixer_label mixer |> sanitize_label in
         edges := { from_id; to_id = target_id; label; style = Routing } :: !edges
       | None -> ())
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

  List.iter (fun t -> add_routing_edge t; add_send_edges t) liveset.tracks;
  List.iter (fun t -> add_routing_edge t; add_send_edges t) liveset.returns;
  add_routing_edge liveset.main;

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
  (!track_info_map, main_node, List.rev !external_list, List.rev !edges, group_info)

let render_nodes_with_groups
    ~(direction : string)
    ~(track_info_map : track_info IntMap.t)
    ~(main_node : node)
    ~(external_nodes : node list)
    ~(edges : edge list)
    ~(group_info : group_info)
  : string =
  let buf = Buffer.create 4096 in
  Buffer.add_string buf (Printf.sprintf "flowchart %s\n" direction);

  let is_group_id id = IntSet.mem id group_info.group_ids in

  let effective_parent id =
    match IntMap.find_opt id group_info.track_parent with
    | Some (Some parent) when is_group_id parent -> Some parent
    | _ -> None
  in

  let children_by_group =
    List.fold_left (fun acc id ->
        match effective_parent id with
        | Some parent ->
          let prev = IntMap.find_opt parent acc |> Option.value ~default:[] in
          IntMap.add parent (id :: prev) acc
        | None -> acc
      ) IntMap.empty group_info.track_order
  in

  let children_of group_id =
    IntMap.find_opt group_id children_by_group
    |> Option.value ~default:[]
    |> List.rev
  in

  let render_node (n : node) =
    Buffer.add_string buf (Printf.sprintf "  %s[\"%s\"]\n" n.id n.label)
  in

  let rec render_group_subgraph visited group_id =
    if IntSet.mem group_id visited then visited
    else
      let visited = IntSet.add group_id visited in
      let subgraph_id = sanitize_id ("group_" ^ string_of_int group_id) in
      let group_label =
        match IntMap.find_opt group_id track_info_map with
        | Some info ->
          (match info.group_label with
           | Some name -> sanitize_label name
           | None -> sanitize_label info.node.label)
        | None -> Printf.sprintf "Group %d" group_id
      in
      Buffer.add_string buf (Printf.sprintf "  subgraph %s[\"%s\"]\n" subgraph_id group_label);
      (match IntMap.find_opt group_id track_info_map with
       | Some info -> render_node info.node
       | None -> ());
      let children = children_of group_id in
      let child_groups, child_tracks =
        List.partition is_group_id children
      in
      List.iter (fun id ->
          match IntMap.find_opt id track_info_map with
          | Some info -> render_node info.node
          | None -> ()
        ) child_tracks;
      let visited =
        List.fold_left (fun v gid -> render_group_subgraph v gid) visited child_groups
      in
      Buffer.add_string buf "  end\n";
      visited
  in

  let top_level_groups =
    IntSet.filter (fun gid -> effective_parent gid = None) group_info.group_ids
    |> IntSet.elements
  in

  let _visited =
    List.fold_left (fun v gid -> render_group_subgraph v gid) IntSet.empty top_level_groups
  in

  let ungrouped_tracks =
    group_info.track_order
    |> List.filter (fun id -> effective_parent id = None && not (is_group_id id))
  in

  List.iter (fun id ->
      match IntMap.find_opt id track_info_map with
      | Some info -> render_node info.node
      | None -> ()
    ) ungrouped_tracks;

  render_node main_node;

  List.iter render_node external_nodes;

  List.iter (fun e ->
      Buffer.add_string buf (Printf.sprintf "  %s\n" (render_edge e))
    ) edges;

  Buffer.contents buf

let render_flowchart ~(xml : Xml.t) ~(liveset : Liveset.t) ~(options : options) : string =
  let track_info_map, main_node, external_nodes, edges, group_info =
    build_graph ~xml ~liveset ~options
  in
  render_nodes_with_groups
    ~direction:options.direction
    ~track_info_map
    ~main_node
    ~external_nodes
    ~edges
    ~group_info
