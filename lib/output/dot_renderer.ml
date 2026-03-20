open Alsdiff_base
open Alsdiff_live
open Flowchart

(* Reuse IntMap and IntSet modules - define locally since Flowchart doesn't expose them *)
module IntMap = Map.Make (Int)
module IntSet = Set.Make (Int)

(* Import all helper functions from Flowchart *)
let sanitize_id = Flowchart.sanitize_id
let mixer_label = Flowchart.mixer_label
let send_label = Flowchart.send_label
let group_subgraph_id = Flowchart.group_subgraph_id

(* DOT-specific label sanitizer - only escapes what DOT needs *)
let sanitize_label (s : string) : string =
  let s = String.map (fun ch -> if ch = '\n' || ch = '\r' then ' ' else ch) s in
  let s = String.concat "\\\\" (String.split_on_char '\\' s) in
  let s = String.concat "\\\"" (String.split_on_char '"' s) in
  s

(* DOT-specific rendering functions *)

let indent ~level ~(s : string) : string =
  let spaces = String.make (level * 2) ' ' in
  spaces ^ s

let render_edge (e : edge) : string =
  let edge_def =
    if String.trim e.label = "" then
      match e.style with
      | Routing -> Printf.sprintf "%s -> %s;" e.from_id e.to_id
      | Send | InputRouting -> Printf.sprintf "%s -> %s [style=dashed];" e.from_id e.to_id
    else
      let sanitized_label = sanitize_label e.label in
      match e.style with
      | Routing -> Printf.sprintf "%s -> %s [label=\"%s\"];" e.from_id e.to_id sanitized_label
      | Send | InputRouting -> Printf.sprintf "%s -> %s [label=\"%s\", style=dashed];" e.from_id e.to_id sanitized_label
  in
  indent ~level:1 ~s:edge_def

let render_node (n : node) : string =
  let node_def = Printf.sprintf "%s [label=\"%s\", shape=box];" n.id (sanitize_label n.label) in
  indent ~level:1 ~s:node_def

let render_node_indented (n : node) : string =
  let node_def = Printf.sprintf "%s [label=\"%s\", shape=box];" n.id (sanitize_label n.label) in
  indent ~level:2 ~s:node_def

let render_subgraph_header (id : string) : string =
  let header = Printf.sprintf "subgraph cluster_%s {" id in
  indent ~level:1 ~s:header

let render_subgraph_label (label : string) : string =
  let label_def = Printf.sprintf "graph [label=\"%s\"];" label in
  indent ~level:2 ~s:label_def

let render_subgraph_footer () : string =
  indent ~level:1 ~s:"}"

let direction_to_rankdir (direction : string) : string =
  match direction with
  | "LR" -> "LR"
  | "TD" -> "TB"
  | _ -> "TB"

let render_nodes_with_groups_dot
    ~(direction : string)
    ~(track_info_map : track_info IntMap.t)
    ~(main_node : node)
    ~(external_nodes : node list)
    ~(edges : edge list)
    ~(group_info : group_info)
  : string =
  let buf = Buffer.create 4096 in
  let rankdir = direction_to_rankdir direction in
  Buffer.add_string buf (Printf.sprintf "digraph G {\n");
  Buffer.add_string buf (Printf.sprintf "  rankdir=%s;\n" rankdir);
  Buffer.add_string buf "  node [shape=box];\n\n";

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

  let rec render_group_subgraph visited group_id =
    if IntSet.mem group_id visited then visited
    else
      let visited = IntSet.add group_id visited in
      let subgraph_id = group_subgraph_id group_id in
      let group_label =
        match IntMap.find_opt group_id track_info_map with
        | Some info ->
          (match info.group_label with
           | Some name -> sanitize_label name
           | None -> sanitize_label info.node.label)
        | None -> Printf.sprintf "Group %d" group_id
      in
      Buffer.add_string buf (render_subgraph_header subgraph_id);
      Buffer.add_string buf "\n";
      Buffer.add_string buf (render_subgraph_label group_label);
      Buffer.add_string buf "\n";

      (match IntMap.find_opt group_id track_info_map with
       | Some info -> Buffer.add_string buf (render_node_indented info.node); Buffer.add_string buf "\n"
       | None -> ());

      let children = children_of group_id in
      let child_groups, child_tracks =
        List.partition is_group_id children
      in

      List.iter (fun id ->
          match IntMap.find_opt id track_info_map with
          | Some info -> Buffer.add_string buf (render_node_indented info.node); Buffer.add_string buf "\n"
          | None -> ()
        ) child_tracks;

      let visited =
        List.fold_left (fun v gid -> render_group_subgraph v gid) visited child_groups
      in
      Buffer.add_string buf (render_subgraph_footer ());
      Buffer.add_string buf "\n";
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
      | Some info -> Buffer.add_string buf (render_node info.node); Buffer.add_string buf "\n"
      | None -> ()
    ) ungrouped_tracks;

  let main_is_connected =
    List.exists (fun e -> e.from_id = main_node.id || e.to_id = main_node.id) edges
  in
  if main_is_connected then (
    Buffer.add_string buf (render_node main_node);
    Buffer.add_string buf "\n"
  );

  List.iter (fun n ->
      Buffer.add_string buf (render_node n);
      Buffer.add_string buf "\n"
    ) external_nodes;

  List.iter (fun e ->
      Buffer.add_string buf (render_edge e);
      Buffer.add_string buf "\n"
    ) edges;

  Buffer.add_string buf "}\n";
  Buffer.contents buf

let render_flowchart ~(xml : Xml.t) ~(liveset : Liveset.t) ~(options : options) : string =
  let flowchart_options = {
    options with
    Flowchart.use_subgraph_id_for_groups = false;
  } in
  let track_info_map, main_node, external_nodes, edges, group_info =
    Flowchart.build_graph ~xml ~liveset ~options:flowchart_options
  in
  render_nodes_with_groups_dot
    ~direction:options.direction
    ~track_info_map
    ~main_node
    ~external_nodes
    ~edges
    ~group_info