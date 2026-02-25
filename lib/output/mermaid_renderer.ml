open Alsdiff_base
open Alsdiff_live
open Flowchart

module IntMap = Map.Make (Int)
module IntSet = Set.Make (Int)

let sanitize_label (s : string) : string =
  let s = String.map (fun ch -> if ch = '\n' || ch = '\r' then ' ' else ch) s in
  let s = String.concat "/" (String.split_on_char '|' s) in
  let s = String.concat "\\\"" (String.split_on_char '"' s) in
  s

let render_edge (e : edge) : string =
  let arrow = match e.style with Routing -> "-->" | Send | InputRouting -> "-.->" in
  if String.trim e.label = "" then
    Printf.sprintf "%s %s %s" e.from_id arrow e.to_id
  else
    Printf.sprintf "%s %s|%s| %s" e.from_id arrow e.label e.to_id

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

  let indent ~(level : int) ~(s : string) : string =
    let spaces = String.make (level * 2) ' ' in
    spaces ^ s
  in

  let render_node ~(level : int) (n : node) =
    let node_def = Printf.sprintf "%s[\"%s\"]" n.id n.label in
    Buffer.add_string buf (indent ~level ~s:node_def);
    Buffer.add_char buf '\n'
  in

  let rec render_group_subgraph ~(level : int) visited group_id =
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
      let subgraph_def = Printf.sprintf "subgraph %s[\"%s\"]" subgraph_id group_label in
      Buffer.add_string buf (indent ~level ~s:subgraph_def);
      Buffer.add_char buf '\n';

      (match IntMap.find_opt group_id track_info_map with
       | Some info -> render_node ~level:(level + 1) info.node
       | None -> ());

      let children = children_of group_id in
      let child_groups, child_tracks =
        List.partition is_group_id children
      in

      List.iter (fun id ->
          match IntMap.find_opt id track_info_map with
          | Some info -> render_node ~level:(level + 1) info.node
          | None -> ()
        ) child_tracks;

      let visited =
        List.fold_left (fun v gid -> render_group_subgraph ~level:(level + 1) v gid) visited child_groups
      in
      Buffer.add_string buf (indent ~level ~s:"end");
      Buffer.add_char buf '\n';
      visited
  in

  let top_level_groups =
    IntSet.filter (fun gid -> effective_parent gid = None) group_info.group_ids
    |> IntSet.elements
  in

  let _visited =
    List.fold_left (fun v gid -> render_group_subgraph ~level:1 v gid) IntSet.empty top_level_groups
  in

  let ungrouped_tracks =
    group_info.track_order
    |> List.filter (fun id -> effective_parent id = None && not (is_group_id id))
  in

  List.iter (fun id ->
      match IntMap.find_opt id track_info_map with
      | Some info -> render_node ~level:1 info.node
      | None -> ()
    ) ungrouped_tracks;

  let main_is_connected =
    List.exists (fun e -> e.from_id = main_node.id || e.to_id = main_node.id) edges
  in
  if main_is_connected then render_node ~level:1 main_node;

  List.iter (fun n -> render_node ~level:1 n) external_nodes;

  List.iter (fun e ->
      Buffer.add_string buf (Printf.sprintf "  %s\n" (render_edge e))
    ) edges;

  Buffer.contents buf

let render_flowchart ~(xml : Xml.t) ~(liveset : Liveset.t) ~(options : options) : string =
  let track_info_map, main_node, external_nodes, edges, group_info =
    Flowchart.build_graph ~xml ~liveset ~options
  in
  render_nodes_with_groups
    ~direction:options.direction
    ~track_info_map
    ~main_node
    ~external_nodes
    ~edges
    ~group_info
