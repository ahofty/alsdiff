open Model

let get_visible_nodes (model : t) : merge_node list =
  let rec visible_helper (nodes : merge_node list) : merge_node list =
    List.filter_map (fun node ->
        let is_expanded = StringSet.mem node.path model.expanded_paths in
        if node.is_expandable && is_expanded then
          Some (node :: visible_helper node.children)
        else
          Some [node]
      ) nodes
    |> List.concat
  in
  visible_helper model.flat_nodes

let move_cursor (model : t) (direction : Msg.t) : t =
  let visible = get_visible_nodes model in
  let max_idx = max 0 (List.length visible - 1) in
  let page_size = model.viewport_height - 2 in
  let new_index = match direction with
    | Msg.MoveUp -> max 0 (model.cursor_index - 1)
    | Msg.MoveDown -> min max_idx (model.cursor_index + 1)
    | Msg.PageUp -> max 0 (model.cursor_index - page_size)
    | Msg.PageDown -> min max_idx (model.cursor_index + page_size)
    | Msg.MoveToStart -> 0
    | Msg.MoveToEnd -> max_idx
    | _ -> model.cursor_index
  in
  { model with cursor_index = new_index }

let toggle_expand (model : t) : t =
  let visible = get_visible_nodes model in
  match List.nth_opt visible model.cursor_index with
  | None -> model
  | Some node ->
    if node.is_expandable then
      let expanded = if StringSet.mem node.path model.expanded_paths
        then StringSet.remove node.path model.expanded_paths
        else StringSet.add node.path model.expanded_paths
      in
      { model with expanded_paths = expanded }
    else model

let rec find_node_by_status (nodes : merge_node list) (status : resolution_status) : merge_node option =
  List.find_opt (fun n ->
      n.status = status || find_node_by_status n.children status <> None
    ) nodes

let find_next_conflict (nodes : merge_node list) (after_path : string) : merge_node option =
  let found = ref false in
  let rec search = function
    | [] -> None
    | n :: rest ->
      if !found && n.status = Unresolved then Some n
      else begin
        if n.path = after_path then found := true;
        match search n.children with
        | Some _ as r -> r
        | None -> search rest
      end
  in
  search nodes

let find_prev_conflict (nodes : merge_node list) (before_path : string) : merge_node option =
  let rec search last_unresolved = function
    | [] -> last_unresolved
    | n :: rest ->
      if n.path = before_path then last_unresolved
      else begin
        let nested = search None n.children in
        let new_last = match nested with
          | Some u -> Some u
          | None ->
            if n.status = Unresolved then Some n else last_unresolved
        in
        search new_last rest
      end
  in
  search None nodes

let jump_to_path (model : t) (path : string) : t =
  let visible = get_visible_nodes model in
  match List.find_index (fun n -> n.path = path) visible with
  | Some idx -> { model with cursor_index = idx }
  | None -> model

let expand_to_path (model : t) (target_path : string) : t =
  let collect_parent_paths path acc =
    let parts = String.split_on_char '/' path in
    let rec build_prefixes parts prefix acc =
      match parts with
      | [] | [_] -> acc
      | p :: rest ->
        let new_prefix = if prefix = "" then p else prefix ^ "/" ^ p in
        build_prefixes rest new_prefix (new_prefix :: acc)
    in
    build_prefixes parts "" acc
  in
  let parent_paths = collect_parent_paths target_path [] in
  let new_expanded = List.fold_left (fun acc p -> StringSet.add p acc)
      model.expanded_paths parent_paths
  in
  { model with expanded_paths = new_expanded }

let next_conflict (model : t) : t =
  let visible = get_visible_nodes model in
  let current = match List.nth_opt visible model.cursor_index with
    | Some n -> n.path
    | None -> ""
  in
  match find_next_conflict model.flat_nodes current with
  | Some node ->
    let model = expand_to_path model node.path in
    jump_to_path model node.path
  | None ->
    match find_node_by_status model.flat_nodes Unresolved with
    | Some node ->
      let model = expand_to_path model node.path in
      jump_to_path model node.path
    | None -> model

let prev_conflict (model : t) : t =
  let visible = get_visible_nodes model in
  let current = match List.nth_opt visible model.cursor_index with
    | Some n -> n.path
    | None -> ""
  in
  match find_prev_conflict model.flat_nodes current with
  | Some node ->
    let model = expand_to_path model node.path in
    jump_to_path model node.path
  | None -> model

let rec update_node_status (path : string) (resolution : Alsdiff_merge.Conflict.resolution)
    (nodes : merge_node list) : merge_node list =
  List.map (fun n ->
      if n.path = path then { n with status = Resolved resolution }
      else { n with children = update_node_status path resolution n.children }
    ) nodes

let resolve_current (model : t) (resolution : Alsdiff_merge.Conflict.resolution) : t =
  let visible = get_visible_nodes model in
  match List.nth_opt visible model.cursor_index with
  | None | Some { status = Auto; _ } -> model
  | Some node ->
    Hashtbl.replace model.resolutions node.path resolution;
    { model with flat_nodes = update_node_status node.path resolution model.flat_nodes }

let rec resolve_all_unresolved (resolution : Alsdiff_merge.Conflict.resolution)
    (nodes : merge_node list) : merge_node list =
  List.map (fun n ->
      match n.status with
      | Unresolved ->
        Hashtbl.replace (Hashtbl.create 0) n.path resolution;
        { n with status = Resolved resolution;
                 children = resolve_all_unresolved resolution n.children }
      | _ ->
        { n with children = resolve_all_unresolved resolution n.children }
    ) nodes

let resolve_all (model : t) (resolution : Alsdiff_merge.Conflict.resolution) : t =
  let rec register_all nodes =
    List.iter (fun n ->
        if n.status = Unresolved then
          Hashtbl.replace model.resolutions n.path resolution;
        register_all n.children
      ) nodes
  in
  register_all model.flat_nodes;
  let rec update_nodes nodes =
    List.map (fun n ->
        let children = update_nodes n.children in
        match n.status with
        | Unresolved -> { n with status = Resolved resolution; children }
        | _ -> { n with children }
      ) nodes
  in
  { model with flat_nodes = update_nodes model.flat_nodes }

let move_left (model : t) : t =
  let visible = get_visible_nodes model in
  match List.nth_opt visible model.cursor_index with
  | None -> model
  | Some node ->
    if StringSet.mem node.path model.expanded_paths then
      { model with expanded_paths = StringSet.remove node.path model.expanded_paths }
    else model

let move_right (model : t) : t =
  let visible = get_visible_nodes model in
  match List.nth_opt visible model.cursor_index with
  | None -> model
  | Some node ->
    if node.is_expandable && not (StringSet.mem node.path model.expanded_paths) then
      { model with expanded_paths = StringSet.add node.path model.expanded_paths }
    else model

let count_resolved (model : t) : int * int =
  let total = ref 0 in
  let resolved = ref 0 in
  let rec count nodes =
    List.iter (fun n ->
        (match n.status with
         | Unresolved -> incr total
         | Resolved _ -> incr total; incr resolved
         | Auto -> ());
        count n.children
      ) nodes
  in
  count model.flat_nodes;
  (!resolved, !total)

let exit_code_ref : int ref = ref 1

let write_merge (model : t) : t * Msg.t Mosaic.Cmd.t =
  let merged_xml = Alsdiff_merge.Merge.apply_context_resolutions
      model.context model.resolutions
  in
  Alsdiff_base.File.write_als model.ours_file merged_xml;
  exit_code_ref := 0;
  (model, Mosaic.Cmd.Quit)

let update (model : t) (msg : Msg.t) : t * Msg.t Mosaic.Cmd.t =
  match model.mode with
  | Help ->
    (match msg with
     | Msg.Resize (w, h) -> ({ model with viewport_width = w; viewport_height = h }, Mosaic.Cmd.none)
     | Msg.ShowHelp -> ({ model with mode = Merge }, Mosaic.Cmd.none)
     | Msg.HideHelp -> ({ model with mode = Merge }, Mosaic.Cmd.none)
     | _ -> ({ model with mode = Merge }, Mosaic.Cmd.none))
  | Merge ->
    (match msg with
     | Msg.MoveUp | Msg.MoveDown | Msg.PageUp | Msg.PageDown
     | Msg.MoveToStart | Msg.MoveToEnd -> (move_cursor model msg, Mosaic.Cmd.none)
     | Msg.ToggleExpand -> (toggle_expand model, Mosaic.Cmd.none)
     | Msg.MoveLeft -> (move_left model, Mosaic.Cmd.none)
     | Msg.MoveRight -> (move_right model, Mosaic.Cmd.none)
     | Msg.NextConflict -> (next_conflict model, Mosaic.Cmd.none)
     | Msg.PrevConflict -> (prev_conflict model, Mosaic.Cmd.none)
     | Msg.ResolveOurs -> (resolve_current model Alsdiff_merge.Conflict.Ours, Mosaic.Cmd.none)
     | Msg.ResolveTheirs -> (resolve_current model Alsdiff_merge.Conflict.Theirs, Mosaic.Cmd.none)
     | Msg.ResolveBase -> (resolve_current model Alsdiff_merge.Conflict.Base, Mosaic.Cmd.none)
     | Msg.ResolveAllOurs -> (resolve_all model Alsdiff_merge.Conflict.Ours, Mosaic.Cmd.none)
     | Msg.ResolveAllTheirs -> (resolve_all model Alsdiff_merge.Conflict.Theirs, Mosaic.Cmd.none)
     | Msg.Write -> write_merge model
     | Msg.ShowHelp -> ({ model with mode = Help }, Mosaic.Cmd.none)
     | Msg.HideHelp -> (model, Mosaic.Cmd.none)
     | Msg.Quit -> (model, Mosaic.Cmd.Quit)
     | Msg.Resize (w, h) -> ({ model with viewport_width = w; viewport_height = h }, Mosaic.Cmd.none))
