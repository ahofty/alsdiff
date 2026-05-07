open Model

(* Check if a string contains a substring (case-insensitive) *)
let contains_substring ~needle (haystack : string) : bool =
  let haystack_lower = String.lowercase_ascii haystack in
  let needle_lower = String.lowercase_ascii needle in
  let nlen = String.length needle_lower in
  let hlen = String.length haystack_lower in
  if nlen = 0 then true
  else if nlen > hlen then false
  else
    let rec loop i =
      i <= hlen - nlen &&
      (String.sub haystack_lower i nlen = needle_lower || loop (i + 1))
    in
    loop 0

(* Filter tree nodes based on search query *)
let rec filter_nodes_by_query ~(query:string) (nodes : Model.tree_node list) : Model.tree_node list =
  List.filter_map (fun node ->
      let label_matches = contains_substring ~needle:query node.label in
      let filtered_children = filter_nodes_by_query ~query node.children in
      let has_matching_children = filtered_children <> [] in
      if label_matches || has_matching_children then
        Some { node with children = if has_matching_children then filtered_children else [] }
      else
        None
    ) nodes

(* Filter tree nodes based on change type *)
let rec filter_nodes_by_change_type
    ~(change_type : Alsdiff_output.View_model.change_type)
    (nodes : Model.tree_node list) : Model.tree_node list =
  List.filter_map (fun node ->
      let change_matches = node.change = change_type in
      let filtered_children =
        filter_nodes_by_change_type ~change_type node.children
      in
      let has_matching_children = filtered_children <> [] in
      if change_matches || has_matching_children then
        Some { node with
               children = if has_matching_children then filtered_children else []
             }
      else None
    ) nodes

(* Filter tree nodes to show only descendants of focused path *)
let filter_nodes_by_focus ~(focus_path:string list) (nodes : Model.tree_node list) : Model.tree_node list =
  match List.find_opt (fun node -> node.path = focus_path) nodes with
  | None -> []
  | Some focus_node -> [focus_node]

let get_visible_nodes (model : Model.t) : Model.tree_node list =
  let nodes_after_search = match model.search_query with
    | Some q when q <> "" -> filter_nodes_by_query ~query:q model.flat_nodes
    | _ -> model.flat_nodes
  in
  let nodes_to_filter = match model.filter_change with
    | Some change_type ->
      filter_nodes_by_change_type ~change_type nodes_after_search
    | None -> nodes_after_search
  in
  let nodes_after_focus = match model.focused_path with
    | Some focus_path -> filter_nodes_by_focus ~focus_path nodes_to_filter
    | None -> nodes_to_filter
  in
  let rec visible_helper (nodes : Model.tree_node list) : Model.tree_node list =
    List.filter_map (fun node ->
        let path_key = String.concat "/" node.path in
        let is_expanded = StringSet.mem path_key model.expanded_paths in
        if node.is_expandable && is_expanded then
          Some (node :: visible_helper node.children)
        else
          Some [node]
      ) nodes
    |> List.concat
  in
  visible_helper nodes_after_focus

let move_cursor (model : Model.t) (direction : Msg.t) : Model.t =
  let visible = get_visible_nodes model in
  let max_idx = max 0 (List.length visible - 1) in
  let page_size = model.viewport_height - 1 in
  let new_index = match direction with
    | Msg.MoveUp -> max 0 (model.cursor_index - 1)
    | Msg.MoveDown -> min max_idx (model.cursor_index + 1)
    | Msg.PageUp -> max 0 (model.cursor_index - page_size)
    | Msg.PageDown -> min max_idx (model.cursor_index + page_size)
    | Msg.MoveToStart -> 0
    | Msg.MoveToEnd -> max_idx
    | _ -> model.cursor_index
  in
  (* Push current position to back history before moving *)
  let get_current_path model =
    let visible = get_visible_nodes model in
    match List.nth_opt visible model.cursor_index with
    | None -> None
    | Some node -> Some node.path
  in
  let new_model = { model with cursor_index = new_index } in
  match direction with
  | Msg.MoveUp | Msg.MoveDown | Msg.PageUp | Msg.PageDown | Msg.MoveToStart | Msg.MoveToEnd ->
    (match get_current_path model with
     | None -> new_model
     | Some path ->
       if model.nav_forward = [] then
         let max_nav_history = 100 in
         let new_back = path :: model.nav_back in
         let trimmed_back =
           if List.length new_back > max_nav_history
           then List.(rev (tl (rev new_back)))
           else new_back
         in
         { new_model with nav_back = trimmed_back; nav_forward = [] }
       else new_model)
  | _ -> new_model

let toggle_expand (model : Model.t) : Model.t =
  let visible = get_visible_nodes model in
  match List.nth_opt visible model.cursor_index with
  | None -> model
  | Some node ->
    if node.is_expandable then
      let path_key = String.concat "/" node.path in
      let expanded = if StringSet.mem path_key model.expanded_paths
        then StringSet.remove path_key model.expanded_paths
        else StringSet.add path_key model.expanded_paths
      in
      { model with expanded_paths = expanded }
    else model

let cycle_detail_mode (model : Model.t) : Model.t =
  let new_idx = (model.detail_mode_index + 1) mod List.length model.detail_modes in
  let new_config = List.nth model.detail_modes new_idx in
  let new_flat_nodes = Model.build_nodes_with_config ~cfg:new_config model.views in
  { model with
    detail_mode_index = new_idx;
    config = new_config;
    flat_nodes = new_flat_nodes;
    cursor_index = min model.cursor_index (max 0 (List.length new_flat_nodes - 1));
  }

let start_search (model : Model.t) : Model.t =
  { model with
    search_mode = true;
    search_query = Some "";
    cursor_index = 0;
  }

let update_search (model : Model.t) (char : string) : Model.t =
  let current_query = match model.search_query with
    | Some q -> q
    | None -> ""
  in
  let new_query = if char = "\127" then
      String.sub current_query 0 (max 0 (String.length current_query - 1))
    else
      current_query ^ char
  in
  { model with search_query = if new_query = "" then None else Some new_query }

let clear_search (model : Model.t) : Model.t =
  { model with
    search_query = None;
    search_mode = false;
    cursor_index = 0;
  }

let end_search (model : Model.t) : Model.t =
  { model with search_mode = false; cursor_index = 0 }

let toggle_change_filter (model : Model.t)
    (_filter : Alsdiff_output.View_model.change_type option) : Model.t =
  let next = match model.filter_change with
    | None -> Some Alsdiff_output.View_model.Added
    | Some Added -> Some Removed
    | Some Removed -> Some Modified
    | Some Modified -> None
    | Some Unchanged -> Some Added
  in
  { model with filter_change = next; cursor_index = 0 }

let jump_to_path (model : Model.t) (path : string list) : Model.t =
  match List.find_opt (fun node -> node.path = path) model.flat_nodes with
  | None -> model
  | Some target_node ->
    (* Helper function to check if one list is a prefix of another *)
    let is_prefix prefix list =
      let rec loop prefix list = match prefix, list with
        | [], _ -> true
        | _, [] -> false
        | x :: xs, y :: ys when x = y -> loop xs ys
        | _ -> false
      in
      loop prefix list
    in
    (* Expand all parent nodes *)
    let expand_parents (nodes : Model.tree_node list) (target : Model.tree_node) : StringSet.t =
      List.fold_left (fun acc node ->
          if is_prefix node.path target.path && node.path <> target.path then
            StringSet.add (String.concat "/" node.path) acc
          else acc
        ) model.expanded_paths nodes
    in
    let expanded = expand_parents model.flat_nodes target_node in
    let visible = get_visible_nodes { model with expanded_paths = expanded } in
    let new_index = match List.find_index (fun n -> n.path = path) visible with
      | Some idx -> idx
      | None -> model.cursor_index
    in
    { model with expanded_paths = expanded; cursor_index = new_index }

let nav_back (model : Model.t) : Model.t =
  match model.nav_back with
  | [] -> model
  | prev_path :: rest ->
    let current_path =
      let visible = get_visible_nodes model in
      match List.nth_opt visible model.cursor_index with
      | None -> None
      | Some node -> Some node.path
    in
    let new_forward = match current_path with
      | None -> model.nav_forward
      | Some p -> p :: model.nav_forward
    in
    jump_to_path { model with nav_back = rest; nav_forward = new_forward } prev_path

let nav_forward (model : Model.t) : Model.t =
  match model.nav_forward with
  | [] -> model
  | next_path :: rest ->
    let current_path =
      let visible = get_visible_nodes model in
      match List.nth_opt visible model.cursor_index with
      | None -> None
      | Some node -> Some node.path
    in
    let new_back = match current_path with
      | None -> model.nav_back
      | Some p -> p :: model.nav_back
    in
    jump_to_path { model with nav_back = new_back; nav_forward = rest } next_path

let browser_move_cursor (model : Model.t) (direction : Msg.t) : Model.t =
  let max_idx = max 0 (List.length model.browser_entries - 1) in
  let page_size = model.viewport_height - 2 in
  let new_idx = match direction with
    | Msg.MoveUp -> max 0 (model.browser_cursor - 1)
    | Msg.MoveDown -> min max_idx (model.browser_cursor + 1)
    | Msg.PageUp -> max 0 (model.browser_cursor - page_size)
    | Msg.PageDown -> min max_idx (model.browser_cursor + page_size)
    | Msg.MoveToStart -> 0
    | Msg.MoveToEnd -> max_idx
    | _ -> model.browser_cursor
  in
  { model with browser_cursor = new_idx }

let browser_go_up (model : Model.t) : Model.t =
  if model.browser_cwd = model.browser_root then model
  else begin
    let parent = Filename.dirname model.browser_cwd in
    let entries = Model.read_dir_entries ~root:model.browser_root ~cwd:parent in
    { model with
      browser_cwd = parent;
      browser_entries = entries;
      browser_cursor = 0;
    }
  end

let browser_activate (model : Model.t) : Model.t * Msg.t Mosaic.Cmd.t =
  match List.nth_opt model.browser_entries model.browser_cursor with
  | None -> (model, Mosaic.Cmd.none)
  | Some entry ->
    if entry.is_dir then
      let entries = Model.read_dir_entries ~root:model.browser_root ~cwd:entry.path in
      ({ model with
         browser_cwd = entry.path;
         browser_entries = entries;
         browser_cursor = 0;
         last_error = None;
       }, Mosaic.Cmd.none)
    else begin
      (* .als file selected *)
      let new_selected =
        if List.mem entry.path model.browser_selected then
          List.filter (fun p -> p <> entry.path) model.browser_selected
        else if List.length model.browser_selected < 2 then
          model.browser_selected @ [entry.path]
        else
          [List.nth model.browser_selected 1; entry.path]
      in
      if List.length new_selected = 2 then begin
        (* Load both files and compute diff *)
        let f1 = List.nth new_selected 0 in
        let f2 = List.nth new_selected 1 in
        (try
           let xml1 = Alsdiff_base.File.open_als f1 in
           let ls1 = Alsdiff_live.Liveset.create xml1 f1 in
           let xml2 = Alsdiff_base.File.open_als f2 in
           let ls2 = Alsdiff_live.Liveset.create xml2 f2 in
           let patch = Alsdiff_live.Liveset.diff ls1 ls2 in
           let has_changes = not (Alsdiff_live.Liveset.Patch.is_empty patch) in
           let change =
             if has_changes then `Modified patch else `Unchanged
           in
           let format_time =
             if Alsdiff_output.View_model.time_format_equal
                 model.time_format Alsdiff_output.View_model.QuarterNotes then
               Alsdiff_output.View_model.default_dual_time_formatter
             else begin
               let main_old = match ls1.Alsdiff_live.Liveset.main with
                 | Alsdiff_live.Track.Main m -> m
                 | _ -> failwith "Liveset.main must be Track.Main"
               in
               let main_new = match ls2.Alsdiff_live.Liveset.main with
                 | Alsdiff_live.Track.Main m -> m
                 | _ -> failwith "Liveset.main must be Track.Main"
               in
               Alsdiff_output.View_model.make_dual_format_time model.time_format
                 ~tempo_events_old:(Alsdiff_live.Track.MainTrack.get_tempo_events main_old)
                 ~ts_events_old:(Alsdiff_live.Track.MainTrack.get_time_signature_events main_old)
                 ~tempo_events_new:(Alsdiff_live.Track.MainTrack.get_tempo_events main_new)
                 ~ts_events_new:(Alsdiff_live.Track.MainTrack.get_time_signature_events main_new)
                 ()
             end
           in
           let views = [
             Alsdiff_output.View_model.Item
               (Alsdiff_output.View_model.create_liveset_item
                  ~note_name_style:model.note_name_style ~format_time
                  ~reference_liveset:ls1 change)
           ] in
           let detail_config = Alsdiff_output.Config.full in
           let flat_nodes = Model.build_nodes_with_config ~cfg:detail_config views in
           ({ model with
              mode = Model.Diff;
              views;
              config = detail_config;
              flat_nodes;
              cursor_index = 0;
              expanded_paths = Model.StringSet.empty;
              search_query = None;
              search_mode = false;
              filter_change = None;
              browser_selected = [];
              last_error = None;
            }, Mosaic.Cmd.none)
         with
         | (Alsdiff_base.File.File_error (_, msg)
           | Alsdiff_base.Xml.Xml_error (_, msg)) ->
           ({ model with last_error = Some msg }, Mosaic.Cmd.none))
      end else
        ({ model with browser_selected = new_selected; last_error = None }, Mosaic.Cmd.none)
    end

let show_export_selector (model : Model.t) : Model.t =
  { model with export_selector_active = true }

let hide_export_selector (model : Model.t) : Model.t =
  { model with export_selector_active = false }

let move_export_selection (model : Model.t) (direction : int) : Model.t =
  let formats = export_formats in
  let current_idx = match List.find_index
                            (fun f -> f = model.export_selected_format) formats with
  | Some idx -> idx
  | None -> 0
  in
  let max_idx = List.length formats - 1 in
  let new_idx = max 0 (min max_idx (current_idx + direction)) in
  { model with export_selected_format = List.nth formats new_idx }

let export_output_ref : string option ref = ref None

let execute_export (model : Model.t) : Model.t * Msg.t Mosaic.Cmd.t =
  let output = try
      match model.export_selected_format with
      | Model.Text ->
        Alsdiff_output.Text_renderer.render model.config model.views
      | Model.Json ->
        Alsdiff_output.Json_renderer.render model.config model.views
      | Model.Statistics ->
        Alsdiff_output.Stats_renderer.render model.config model.views
    with
    | e ->
      Format.eprintf "Export failed: %s\n" (Printexc.to_string e);
      "Export error - see terminal output"
  in
  export_output_ref := Some output;
  ({ model with export_selector_active = false }, Mosaic.Cmd.Quit)

let enter_focus (model : Model.t) : Model.t =
  let visible = get_visible_nodes model in
  match List.nth_opt visible model.cursor_index with
  | None -> model
  | Some node ->
    if node.is_expandable then
      { model with focused_path = Some node.path; cursor_index = 0 }
    else model

let exit_focus (model : Model.t) : Model.t =
  { model with focused_path = None; cursor_index = 0 }

let get_parent_path (path : string list) : string list option =
  match List.rev path with
  | [] | [_] -> None
  | _ :: rest -> Some (List.rev rest)

let move_left (model : Model.t) : Model.t =
  let visible = get_visible_nodes model in
  match List.nth_opt visible model.cursor_index with
  | None -> model
  | Some node ->
    let path_key = String.concat "/" node.path in
    if node.is_expandable && StringSet.mem path_key model.expanded_paths then
      (* Collapse expanded node *)
      { model with expanded_paths = StringSet.remove path_key model.expanded_paths }
    else
      (* Jump to parent node *)
      match get_parent_path node.path with
      | None -> model
      | Some parent_path ->
        match List.find_index (fun n -> n.path = parent_path) visible with
        | None -> model
        | Some idx -> { model with cursor_index = idx }

let move_right (model : Model.t) : Model.t =
  let visible = get_visible_nodes model in
  match List.nth_opt visible model.cursor_index with
  | None -> model
  | Some node ->
    let path_key = String.concat "/" node.path in
    if node.is_expandable && not (StringSet.mem path_key model.expanded_paths) then
      (* Expand collapsed expandable node *)
      { model with expanded_paths = StringSet.add path_key model.expanded_paths }
    else if node.is_expandable then
      (* Move cursor to first child *)
      let child_path = node.path in
      match List.find_index (fun n ->
          let rec is_prefix prefix lst = match prefix, lst with
            | [], _ -> true
            | _, [] -> false
            | p :: ps, x :: xs when p = x -> is_prefix ps xs
            | _ -> false
          in is_prefix child_path n.path && n.path <> child_path
        ) visible with
      | Some idx -> { model with cursor_index = idx }
      | None -> model
    else model

let update (model : Model.t) (msg : Msg.t) : Model.t * Msg.t Mosaic.Cmd.t =
  match model.mode with
  | Model.Help | Model.Stats ->
    (match msg with
     | Msg.Resize (w, h) ->
       ({ model with viewport_width = w; viewport_height = h }, Mosaic.Cmd.none)
     | _ ->
       let return_mode = match model.previous_mode with
         | Some m -> m
         | None -> Model.Diff
       in
       ({ model with mode = return_mode; previous_mode = None }, Mosaic.Cmd.none))
  | Model.Browser ->
    (match msg with
     | Msg.ShowHelp | Msg.ToggleHelp -> ({ model with mode = Model.Help; previous_mode = Some model.mode }, Mosaic.Cmd.none)
     | Msg.ShowStats | Msg.ToggleStats -> ({ model with mode = Model.Stats; previous_mode = Some model.mode }, Mosaic.Cmd.none)
     | Msg.MoveUp | Msg.MoveDown | Msg.PageUp | Msg.PageDown | Msg.MoveToStart | Msg.MoveToEnd -> (browser_move_cursor model msg, Mosaic.Cmd.none)
     | Msg.BrowserActivate -> browser_activate model
     | Msg.BrowserGoUp -> (browser_go_up model, Mosaic.Cmd.none)
     | Msg.Resize (w, h) -> ({ model with viewport_width = w; viewport_height = h }, Mosaic.Cmd.none)
     | Msg.Quit -> (model, Mosaic.Cmd.Quit)
     | Msg.HideHelp | Msg.HideStats -> (model, Mosaic.Cmd.none)
     | _ -> (model, Mosaic.Cmd.none))
  | Model.Diff ->
    (match msg with
     | Msg.ShowExportSelector -> (show_export_selector model, Mosaic.Cmd.none)
     | Msg.HideExportSelector -> (hide_export_selector model, Mosaic.Cmd.none)
     | Msg.MoveExportSelection dir ->
       if model.export_selector_active
       then (move_export_selection model dir, Mosaic.Cmd.none)
       else (model, Mosaic.Cmd.none)
     | Msg.ExecuteExport ->
       execute_export model
     | Msg.ShowHelp | Msg.ToggleHelp -> ({ model with mode = Model.Help; previous_mode = Some model.mode }, Mosaic.Cmd.none)
     | Msg.ShowStats | Msg.ToggleStats -> ({ model with mode = Model.Stats; previous_mode = Some model.mode }, Mosaic.Cmd.none)
     | Msg.NavBack -> (nav_back model, Mosaic.Cmd.none)
     | Msg.NavForward -> (nav_forward model, Mosaic.Cmd.none)
     | Msg.EnterFocus -> (enter_focus model, Mosaic.Cmd.none)
     | Msg.ExitFocus -> (exit_focus model, Mosaic.Cmd.none)
     | Msg.MoveUp | Msg.MoveDown | Msg.PageUp | Msg.PageDown | Msg.MoveToStart | Msg.MoveToEnd -> (move_cursor model msg, Mosaic.Cmd.none)
     | Msg.MoveLeft -> (move_left model, Mosaic.Cmd.none)
     | Msg.MoveRight -> (move_right model, Mosaic.Cmd.none)
     | Msg.ToggleExpand -> (toggle_expand model, Mosaic.Cmd.none)
     | Msg.CycleDetailMode ->
       let new_model = cycle_detail_mode model in
       (new_model, Mosaic.Cmd.none)
     | Msg.StartSearch -> (start_search model, Mosaic.Cmd.none)
     | Msg.UpdateSearch query -> (update_search model query, Mosaic.Cmd.none)
     | Msg.ClearSearch -> (clear_search model, Mosaic.Cmd.none)
     | Msg.EndSearch -> (end_search model, Mosaic.Cmd.none)
     | Msg.ToggleChangeFilter filter -> (toggle_change_filter model filter, Mosaic.Cmd.none)
     | Msg.JumpToPath path -> (jump_to_path model path, Mosaic.Cmd.none)
     | Msg.Resize (w, h) -> ({ model with viewport_width = w; viewport_height = h }, Mosaic.Cmd.none)
     | Msg.Quit ->
       if model.browser_root <> "" then
         (* Return to browser mode *)
         let entries =
           Model.read_dir_entries ~root:model.browser_root ~cwd:model.browser_cwd
         in
         ({ model with
            mode = Model.Browser;
            browser_entries = entries;
            browser_cursor = 0;
            browser_selected = [];
          }, Mosaic.Cmd.none)
       else
         (model, Mosaic.Cmd.Quit)
     | Msg.BackToBrowser ->
       (match model.focused_path with
        | Some _ ->
          (* Exit focus mode first *)
          ({ model with focused_path = None; cursor_index = 0 }, Mosaic.Cmd.none)
        | None ->
          if model.browser_root <> "" then
            let entries =
              Model.read_dir_entries ~root:model.browser_root ~cwd:model.browser_cwd
            in
            ({ model with
               mode = Model.Browser;
               browser_entries = entries;
               browser_cursor = 0;
               browser_selected = [];
             }, Mosaic.Cmd.none)
          else
            (model, Mosaic.Cmd.Quit))
     | Msg.BrowserActivate | Msg.BrowserGoUp | Msg.HideHelp | Msg.HideStats -> (model, Mosaic.Cmd.none))
