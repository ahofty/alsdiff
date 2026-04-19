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

let rec propagate_parent_status (nodes : merge_node list) : merge_node list =
  List.map (fun n ->
      let children = propagate_parent_status n.children in
      let parent_status =
        if children = [] then n.status
        else
          let child_statuses = List.map (fun c -> c.status) children in
          let all_resolved = List.for_all (function
              | Resolved _ | Auto | Mixed_resolved -> true
              | Unresolved -> false
            ) child_statuses
          in
          let any_resolved = List.exists (function
              | Resolved _ | Mixed_resolved -> true
              | Auto | Unresolved -> false
            ) child_statuses
          in
          if all_resolved && any_resolved then
            let resolutions = List.filter_map (function
                | Resolved r -> Some r | _ -> None
              ) child_statuses
            in
            let all_same = match resolutions with
              | [] | [_] -> true
              | first :: rest -> List.for_all (fun r -> r = first) rest
            in
            if all_same && List.length resolutions = List.length child_statuses then
              Resolved (List.hd resolutions)
            else
              Mixed_resolved
          else n.status
      in
      { n with status = parent_status; children }
    ) nodes

let resolve_current (model : t) (resolution : Alsdiff_merge.Conflict.resolution) : t =
  let visible = get_visible_nodes model in
  match List.nth_opt visible model.cursor_index with
  | None -> model
  | Some node ->
    Hashtbl.replace model.resolutions node.path resolution;
    let flat_nodes = update_node_status node.path resolution model.flat_nodes in
    { model with flat_nodes = propagate_parent_status flat_nodes }

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
         | Resolved _ | Mixed_resolved -> incr total; incr resolved
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

let resolve_debug model label =
  let visible = get_visible_nodes model in
  match List.nth_opt visible model.cursor_index with
  | None -> Fmt.str "no node at cursor (%d/%d)" model.cursor_index (List.length visible)
  | Some { status = Auto; path = p; _ } -> Fmt.str "override auto %s as %s" p label
  | Some { status = Unresolved; path = p; _ } -> Fmt.str "resolve %s as %s" p label
  | Some { status = Resolved _; path = p; _ } -> Fmt.str "re-resolve %s as %s" p label
  | Some { status = Mixed_resolved; path = p; _ } -> Fmt.str "re-resolve %s as %s" p label

let rec handle_cherry_pick_msg (model : t) (msg : Msg.t) : t * Msg.t Mosaic.Cmd.t =
  match model.cherry_pick, msg with
  | None, _ -> ({ model with mode = Merge }, Mosaic.Cmd.none)
  | Some _, Msg.Resize (w, h) ->
    ({ model with viewport_width = w; viewport_height = h }, Mosaic.Cmd.none)
  | Some _, Msg.LeaveCherryPick ->
    ({ model with mode = Merge; cherry_pick = None; last_action = "" }, Mosaic.Cmd.none)
  | Some cp, Msg.SelectFieldOurs ->
    let selections = List.mapi (fun i (name, r) ->
        if i = cp.cursor_field then (name, Alsdiff_merge.Conflict.Ours)
        else (name, r)
      ) cp.field_selections in
    ({ model with cherry_pick = Some { cp with field_selections = selections } },
     Mosaic.Cmd.none)
  | Some cp, Msg.SelectFieldTheirs ->
    let selections = List.mapi (fun i (name, r) ->
        if i = cp.cursor_field then (name, Alsdiff_merge.Conflict.Theirs)
        else (name, r)
      ) cp.field_selections in
    ({ model with cherry_pick = Some { cp with field_selections = selections } },
     Mosaic.Cmd.none)
  | Some cp, Msg.SelectFieldBase ->
    let selections = List.mapi (fun i (name, r) ->
        if i = cp.cursor_field then (name, Alsdiff_merge.Conflict.Base)
        else (name, r)
      ) cp.field_selections in
    ({ model with cherry_pick = Some { cp with field_selections = selections } },
     Mosaic.Cmd.none)
  | Some cp, Msg.CherryPickNextField ->
    let max_field = max 0 (List.length cp.field_selections - 1) in
    let cursor = min max_field (cp.cursor_field + 1) in
    ({ model with cherry_pick = Some { cp with cursor_field = cursor } },
     Mosaic.Cmd.none)
  | Some cp, Msg.CherryPickPrevField ->
    let cursor = max 0 (cp.cursor_field - 1) in
    ({ model with cherry_pick = Some { cp with cursor_field = cursor } },
     Mosaic.Cmd.none)
  | Some cp, Msg.ApplyCherryPick ->
    List.iter (fun (field_name, resolution) ->
        let field_path = cp.entity_path ^ "/" ^ field_name in
        Hashtbl.replace model.resolutions field_path resolution
      ) cp.field_selections;
    let model = { model with mode = Merge; cherry_pick = None;
                             last_action = "cherry-pick applied" } in
    let model = { model with flat_nodes = propagate_parent_status model.flat_nodes } in
    (model, Mosaic.Cmd.none)
  | Some _, (Msg.ResolveOurs | Msg.MoveUp) ->
    handle_cherry_pick_msg model Msg.SelectFieldOurs
  | Some _, (Msg.ResolveTheirs | Msg.MoveDown) ->
    handle_cherry_pick_msg model Msg.SelectFieldTheirs
  | Some _, Msg.ResolveBase ->
    handle_cherry_pick_msg model Msg.SelectFieldBase
  | Some _, (Msg.NextConflict | Msg.ToggleExpand) ->
    handle_cherry_pick_msg model Msg.CherryPickNextField
  | Some _, Msg.PrevConflict ->
    handle_cherry_pick_msg model Msg.CherryPickPrevField
  | Some _, (Msg.Quit | Msg.Write) ->
    handle_cherry_pick_msg model Msg.ApplyCherryPick
  | Some _, _ -> (model, Mosaic.Cmd.none)

let handle_merge_msg (model : t) (msg : Msg.t) : t * Msg.t Mosaic.Cmd.t =
  match msg with
  | Msg.MoveUp | Msg.MoveDown | Msg.PageUp | Msg.PageDown
  | Msg.MoveToStart | Msg.MoveToEnd ->
    let model = { model with last_action = "" } in
    (move_cursor model msg, Mosaic.Cmd.none)
  | Msg.ToggleExpand ->
    let visible = get_visible_nodes model in
    let model = { model with last_action = "" } in
    (match List.nth_opt visible model.cursor_index with
     | Some { ours_desc = Some _; _ } as node ->
       ({ model with mode = Detail; detail_node = node }, Mosaic.Cmd.none)
     | _ -> (toggle_expand model, Mosaic.Cmd.none))
  | Msg.MoveLeft ->
    let model = { model with last_action = "" } in
    (move_left model, Mosaic.Cmd.none)
  | Msg.MoveRight ->
    let model = { model with last_action = "" } in
    (move_right model, Mosaic.Cmd.none)
  | Msg.NextConflict ->
    let model = { model with last_action = "" } in
    (next_conflict model, Mosaic.Cmd.none)
  | Msg.PrevConflict ->
    let model = { model with last_action = "" } in
    (prev_conflict model, Mosaic.Cmd.none)
  | Msg.ResolveOurs ->
    let debug = resolve_debug model "ours" in
    let model' = resolve_current model Alsdiff_merge.Conflict.Ours in
    ({ model' with last_action = debug }, Mosaic.Cmd.none)
  | Msg.ResolveTheirs ->
    let debug = resolve_debug model "theirs" in
    let model' = resolve_current model Alsdiff_merge.Conflict.Theirs in
    ({ model' with last_action = debug }, Mosaic.Cmd.none)
  | Msg.ResolveBase ->
    let debug = resolve_debug model "base" in
    let model' = resolve_current model Alsdiff_merge.Conflict.Base in
    ({ model' with last_action = debug }, Mosaic.Cmd.none)
  | Msg.ResolveAllOurs ->
    let debug = "resolve all as ours" in
    let model' = resolve_all model Alsdiff_merge.Conflict.Ours in
    ({ model' with last_action = debug }, Mosaic.Cmd.none)
  | Msg.ResolveAllTheirs ->
    let debug = "resolve all as theirs" in
    let model' = resolve_all model Alsdiff_merge.Conflict.Theirs in
    ({ model' with last_action = debug }, Mosaic.Cmd.none)
  | Msg.Write -> write_merge model
  | Msg.ShowHelp -> ({ model with mode = Help; last_action = "" }, Mosaic.Cmd.none)
  | Msg.HideHelp -> ({ model with last_action = "" }, Mosaic.Cmd.none)
  | Msg.Quit -> (model, Mosaic.Cmd.Quit)
  | Msg.Resize (w, h) -> ({ model with viewport_width = w; viewport_height = h }, Mosaic.Cmd.none)
  | Msg.ToggleView -> ({ model with mode = SideBySide; last_action = "" }, Mosaic.Cmd.none)
  | Msg.EnterCherryPick ->
    (match List.nth_opt (get_visible_nodes model) model.cursor_index with
     | Some { entity_data = Some edata; path; _ } ->
       let base_xml = Option.value edata.base_xml
           ~default:(Alsdiff_base.Xml.Element { name = "none"; attrs = []; childs = [] }) in
       let ours_xml = Option.value edata.ours_xml
           ~default:(Alsdiff_base.Xml.Element { name = "none"; attrs = []; childs = [] }) in
       let theirs_xml = Option.value edata.theirs_xml
           ~default:(Alsdiff_base.Xml.Element { name = "none"; attrs = []; childs = [] }) in
       let field_diffs = Alsdiff_merge.Xml_compare.compare_three_way
           ~base:base_xml ~ours:ours_xml ~theirs:theirs_xml in
       let field_selections = List.map (fun (d : Alsdiff_merge.Xml_compare.field_diff) ->
           let field_path = path ^ "/" ^ d.Alsdiff_merge.Xml_compare.field_name in
           let resolution = match Hashtbl.find_opt model.resolutions field_path with
             | Some r -> r
             | None -> Alsdiff_merge.Conflict.Ours
           in
           (d.Alsdiff_merge.Xml_compare.field_name, resolution)
         ) field_diffs in
       let state : cherry_pick_state = {
         entity_path = path;
         field_selections;
         field_diffs;
         cursor_field = 0;
       } in
       ({ model with mode = CherryPick; cherry_pick = Some state; last_action = "cherry-pick" },
        Mosaic.Cmd.none)
     | _ -> ({ model with last_action = "no entity data" }, Mosaic.Cmd.none))
  | (Msg.LeaveCherryPick | Msg.SelectFieldOurs | Msg.SelectFieldTheirs
    | Msg.SelectFieldBase | Msg.CherryPickNextField | Msg.CherryPickPrevField
    | Msg.ApplyCherryPick) ->
    (model, Mosaic.Cmd.none)

let update (model : t) (msg : Msg.t) : t * Msg.t Mosaic.Cmd.t =
  match model.mode with
  | Help ->
    (match msg with
     | Msg.Resize (w, h) -> ({ model with viewport_width = w; viewport_height = h }, Mosaic.Cmd.none)
     | Msg.ShowHelp -> ({ model with mode = Merge }, Mosaic.Cmd.none)
     | Msg.HideHelp -> ({ model with mode = Merge }, Mosaic.Cmd.none)
     | _ -> ({ model with mode = Merge }, Mosaic.Cmd.none))
  | Detail ->
    (match msg with
     | Msg.Resize (w, h) -> ({ model with viewport_width = w; viewport_height = h }, Mosaic.Cmd.none)
     | _ -> ({ model with mode = Merge; detail_node = None }, Mosaic.Cmd.none))
  | Merge -> handle_merge_msg model msg
  | SideBySide ->
    (match msg with
     | Msg.ToggleView -> ({ model with mode = Merge }, Mosaic.Cmd.none)
     | _ -> handle_merge_msg model msg)
  | CherryPick ->
    handle_cherry_pick_msg model msg
