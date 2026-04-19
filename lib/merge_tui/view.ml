open Model

let truncate_to_width ~width text =
  let display_len =
    let n = ref 0 in
    String.iter (fun c -> if Char.code c land 0xC0 <> 0x80 then incr n) text;
    !n
  in
  if display_len <= width then text
  else
    let buf = Buffer.create width in
    let chars = ref 0 in
    String.iter (fun c ->
        if !chars >= width - 1 then ()
        else begin
          Buffer.add_char buf c;
          if Char.code c land 0xC0 <> 0x80 then incr chars
        end
      ) text;
    Buffer.add_string buf "\xe2\x80\xa6";
    Buffer.contents buf

let pad_to_width ~width text =
  let display_len =
    let n = ref 0 in
    String.iter (fun c -> if Char.code c land 0xC0 <> 0x80 then incr n) text;
    !n
  in
  if display_len >= width then text
  else text ^ String.make (width - display_len) ' '

let render_node ~(model : t) ~(is_cursor : bool) (node : merge_node) : Msg.t Mosaic.t =
  let prefix = String.make (node.depth * 2) ' ' in
  let expand_indicator = if node.is_expandable then
      if StringSet.mem node.path model.expanded_paths then "\xe2\x96\xbc" else "\xe2\x96\xb6"
    else " "
  in
  let cursor_str = if is_cursor then "\xe2\x96\xba " else "  " in
  let status_label, change_style = match node.status with
    | Auto ->
      (" [auto]", Mosaic.Ansi.Style.(with_dim true default))
    | Unresolved ->
      ("", Mosaic.Ansi.Style.(with_bold true (fg Mosaic.Ansi.Color.red default)))
    | Resolved resolution ->
      let label = match resolution with
        | Alsdiff_merge.Conflict.Ours -> " [ours]"
        | Theirs -> " [theirs]"
        | Base -> " [base]"
      in
      (label, Mosaic.Ansi.Style.(fg Mosaic.Ansi.Color.green default))
    | Mixed_resolved ->
      (" [mixed]", Mosaic.Ansi.Style.(fg Mosaic.Ansi.Color.yellow default))
  in
  let text = Fmt.str "%s%s%s%s%s" cursor_str prefix expand_indicator node.label status_label in
  let style = if is_cursor
    then Mosaic.Ansi.Style.(change_style ++ fg Mosaic.Ansi.Color.cyan default)
    else change_style
  in
  Mosaic.text ~style text

let render_status_bar (model : t) : Msg.t Mosaic.t =
  let resolved, total = Update.count_resolved model in
  let conflicts = List.length model.context.conflicts in
  let status = Fmt.str
      " %d/%d resolved | %d conflicts | %s | j/k:nav Tab:next v:view a/t/b:resolve w:write q:quit "
      resolved total conflicts model.last_action
  in
  let bar_style = Mosaic.Ansi.Style.(bg Mosaic.Ansi.Color.blue default) in
  Mosaic.text ~style:bar_style (pad_to_width ~width:model.viewport_width status)

let render_help (_model : t) : Msg.t Mosaic.t =
  let open Mosaic.Ansi.Style in
  let title_style = fg Mosaic.Ansi.Color.yellow default in
  let title = Mosaic.text ~style:title_style "alsmerge - Help" in
  let section_style = fg Mosaic.Ansi.Color.cyan default in
  let key_style = fg Mosaic.Ansi.Color.green default in
  let sections = [
    ("Navigation", [
        ("j / k / Up / Down", "Move up/down");
        ("h / l / Left / Right", "Collapse/Expand");
        ("PageUp / PageDown", "Page up/down");
        ("Home / End", "Jump to start/end");
        ("Tab / Shift+Tab", "Next/Prev conflict");
      ]);
    ("Resolution", [
        ("a", "Resolve as ours");
        ("t", "Resolve as theirs");
        ("b", "Resolve as base");
        ("A", "Resolve ALL as ours");
        ("T", "Resolve ALL as theirs");
      ]);
    ("Actions", [
        ("Space / Enter", "Expand / Show conflict detail");
        ("v", "Toggle side-by-side view");
        ("w", "Write merged file and quit");
        ("?", "Show this help");
        ("q", "Quit without saving");
      ]);
  ] in
  let render_section (section_name, bindings) =
    let header = Mosaic.text ~style:section_style (Fmt.str "\n%s:" section_name) in
    let bindings_list = List.map (fun (key, desc) ->
        let key_text = Mosaic.text ~style:key_style (Fmt.str "  %-22s" key) in
        let desc_text = Mosaic.text desc in
        Mosaic.box ~flex_direction:Mosaic.Flex_direction.Row [key_text; desc_text]
      ) bindings in
    Mosaic.box ~flex_direction:Mosaic.Flex_direction.Column (header :: bindings_list)
  in
  let sections_view = List.map render_section sections in
  let footer = Mosaic.text
      ~style:(fg Mosaic.Ansi.Color.blue default) "\nPress any key to close" in
  Mosaic.box ~flex_direction:Mosaic.Flex_direction.Column (title :: sections_view @ [footer])

let conflict_kind_to_string = function
  | Alsdiff_merge.Conflict.Atomic_conflict -> "Atomic conflict"
  | Alsdiff_merge.Conflict.Add_remove_conflict -> "Add/remove conflict"
  | Alsdiff_merge.Conflict.Overlapping_modification -> "Overlapping modification"

let render_detail (model : t) : Msg.t Mosaic.t =
  let open Mosaic.Ansi.Style in
  let node = match model.detail_node with Some n -> n | None -> assert false in
  let kind_text = match node.conflict_kind with
    | Some k -> conflict_kind_to_string k
    | None -> "Unknown"
  in
  let status_text = match node.status with
    | Auto -> "Auto-resolved"
    | Unresolved -> "Unresolved"
    | Resolved Alsdiff_merge.Conflict.Ours -> "Resolved (ours)"
    | Resolved Alsdiff_merge.Conflict.Theirs -> "Resolved (theirs)"
    | Resolved Alsdiff_merge.Conflict.Base -> "Resolved (base)"
    | Mixed_resolved -> "Mixed resolution"
  in
  let ours_text = match node.ours_desc with Some t -> t | None -> "N/A" in
  let theirs_text = match node.theirs_desc with Some t -> t | None -> "N/A" in
  let title = Mosaic.text ~style:(fg Mosaic.Ansi.Color.yellow default) "Conflict Detail" in
  let path_line = Mosaic.text (Fmt.str "
Path: %s" node.path) in
  let kind_line = Mosaic.text (Fmt.str "Kind: %s" kind_text) in
  let status_style = match node.status with
    | Unresolved -> fg Mosaic.Ansi.Color.red default
    | Resolved _ -> fg Mosaic.Ansi.Color.green default
    | Auto -> with_dim true default
    | Mixed_resolved -> fg Mosaic.Ansi.Color.yellow default
  in
  let status_line = Mosaic.text ~style:status_style (Fmt.str "Status: %s" status_text) in
  let ours_label = Mosaic.text ~style:(fg Mosaic.Ansi.Color.green default)
      "
Ours:" in
  let ours_body = Mosaic.text ("  " ^ ours_text) in
  let theirs_label = Mosaic.text ~style:(fg Mosaic.Ansi.Color.red default)
      "
Theirs:" in
  let theirs_body = Mosaic.text ("  " ^ theirs_text) in
  let footer = Mosaic.text
      ~style:(fg Mosaic.Ansi.Color.blue default) "

Press any key to close" in
  Mosaic.box ~flex_direction:Mosaic.Flex_direction.Column [
    title; path_line; kind_line; status_line;
    ours_label; ours_body; theirs_label; theirs_body; footer
  ]

let present_in_version (node : merge_node) = function
  | `Ours -> node.presence.ours
  | `Base -> node.presence.base
  | `Theirs -> node.presence.theirs

let cell_style (node : merge_node) version =
  let open Mosaic.Ansi.Style in
  let present = present_in_version node version in
  if not present then with_dim true default
  else match node.status with
    | Auto -> with_dim true default
    | Unresolved ->
      (match node.conflict_kind with
       | Some _ -> with_bold true (fg Mosaic.Ansi.Color.red default)
       | None -> fg Mosaic.Ansi.Color.yellow default)
    | Resolved resolution ->
      let chosen = match resolution, version with
        | Alsdiff_merge.Conflict.Ours, `Ours
        | Theirs, `Theirs
        | Base, `Base -> true
        | _ -> false
      in
      if chosen
      then with_bold true (fg Mosaic.Ansi.Color.green default)
      else with_dim true default
    | Mixed_resolved -> fg Mosaic.Ansi.Color.yellow default

let cell_text (node : merge_node) ~(expanded : StringSet.t) ~col_width version =
  let prefix = String.make (node.depth * 2) ' ' in
  let expand = if node.is_expandable then
      if StringSet.mem node.path expanded then "\xe2\x96\xbc " else "\xe2\x96\xb6 "
    else "  "
  in
  let name = if node.name = "" then node.label else node.name in
  let present = present_in_version node version in
  let resolution_label = match node.status with
    | Resolved Alsdiff_merge.Conflict.Ours -> " [ours]"
    | Resolved Theirs -> " [theirs]"
    | Resolved Base -> " [base]"
    | _ -> ""
  in
  let raw = if present
    then prefix ^ expand ^ name ^ resolution_label
    else String.make col_width ' '
  in
  let text = truncate_to_width ~width:col_width raw in
  pad_to_width ~width:col_width text

let render_side_by_side (model : t) : Msg.t Mosaic.t =
  let visible_nodes = Update.get_visible_nodes model in
  let total = List.length visible_nodes in
  let max_rows = max 1 (model.viewport_height - 2) in
  let scroll_offset =
    if total <= max_rows then 0
    else
      let half = max_rows / 2 in
      max 0 (min (model.cursor_index - half) (total - max_rows))
  in
  let visible_slice =
    List.drop scroll_offset visible_nodes
    |> List.take max_rows
  in
  let col_width = max 10 ((model.viewport_width - 2) / 3) in
  let sep = Mosaic.text ~style:Mosaic.Ansi.Style.(with_dim true default) "\xe2\x94\x82" in
  let header_style = Mosaic.Ansi.Style.(with_bold true (fg Mosaic.Ansi.Color.yellow default)) in
  let header_ours = Mosaic.text ~style:header_style
      (pad_to_width ~width:col_width (truncate_to_width ~width:col_width "Ours")) in
  let header_base = Mosaic.text ~style:header_style
      (pad_to_width ~width:col_width (truncate_to_width ~width:col_width "Base")) in
  let header_theirs = Mosaic.text ~style:header_style
      (pad_to_width ~width:col_width (truncate_to_width ~width:col_width "Theirs")) in
  let header_row = Mosaic.box ~flex_direction:Mosaic.Flex_direction.Row
      [header_ours; sep; header_base; sep; header_theirs] in
  let rows = List.mapi (fun i node ->
      let global_i = scroll_offset + i in
      let is_cursor = global_i = model.cursor_index in
      let cursor_bg = if is_cursor
        then Some Mosaic.Ansi.Color.blue
        else None
      in
      let make_cell version =
        let text = cell_text node ~expanded:model.expanded_paths ~col_width version in
        let style = cell_style node version in
        let style = match cursor_bg with
          | Some _bg -> Mosaic.Ansi.Style.(style ++ fg Mosaic.Ansi.Color.cyan default)
          | None -> style
        in
        Mosaic.text ~style text
      in
      Mosaic.box ~flex_direction:Mosaic.Flex_direction.Row
        [make_cell `Ours; sep; make_cell `Base; sep; make_cell `Theirs]
    ) visible_slice
  in
  let num_blank_rows = max_rows - List.length visible_slice in
  let blank_lines = if num_blank_rows > 0
    then List.map (fun _ -> Mosaic.text "") (Array.to_list (Array.make num_blank_rows ()))
    else []
  in
  let resolved, total_conflicts = Update.count_resolved model in
  let conflicts = List.length model.context.conflicts in
  let status = Fmt.str
      " %d/%d resolved | %d conflicts | %s | v:view | j/k:nav a/t/b:resolve w:write q:quit "
      resolved total_conflicts conflicts model.last_action
  in
  let bar_style = Mosaic.Ansi.Style.(bg Mosaic.Ansi.Color.blue default) in
  let status_bar = Mosaic.text ~style:bar_style
      (pad_to_width ~width:model.viewport_width status) in
  Mosaic.box ~flex_direction:Mosaic.Flex_direction.Column [
    header_row;
    Mosaic.box ~flex_direction:Mosaic.Flex_direction.Column (rows @ blank_lines);
    status_bar;
  ]

let render_merge (model : t) : Msg.t Mosaic.t =
  let visible_nodes = Update.get_visible_nodes model in
  let total = List.length visible_nodes in
  let max_rows = max 1 (model.viewport_height - 1) in
  let scroll_offset =
    if total <= max_rows then 0
    else
      let half = max_rows / 2 in
      max 0 (min (model.cursor_index - half) (total - max_rows))
  in
  let visible_slice =
    List.drop scroll_offset visible_nodes
    |> List.take max_rows
  in
  let nodes_box = List.mapi (fun i node ->
      let global_i = scroll_offset + i in
      render_node ~model ~is_cursor:(global_i = model.cursor_index) node
    ) visible_slice
  in
  let num_blank_rows = max_rows - List.length visible_slice in
  let blank_lines = if num_blank_rows > 0
    then List.map (fun _ -> Mosaic.text "") (Array.to_list (Array.make num_blank_rows ()))
    else []
  in
  Mosaic.box ~flex_direction:Mosaic.Flex_direction.Column [
    Mosaic.box ~flex_direction:Mosaic.Flex_direction.Column (nodes_box @ blank_lines);
    render_status_bar model;
  ]

let render_cherry_pick (model : t) : Msg.t Mosaic.t =
  let open Mosaic.Ansi.Style in
  match model.cherry_pick with
  | None -> Mosaic.text "No cherry-pick data"
  | Some cp ->
    let title = Mosaic.text ~style:(fg Mosaic.Ansi.Color.yellow default)
        (Fmt.str "Cherry-Pick: %s" cp.entity_path) in
    let header_style = with_bold true (fg Mosaic.Ansi.Color.yellow default) in
    let col_w = max 10 ((model.viewport_width - 4) / 4) in
    let headers = [
      "Base"; "Ours"; "Theirs"; "Selection"
    ] in
    let header_row = Mosaic.box ~flex_direction:Mosaic.Flex_direction.Row
        (List.map (fun h ->
             Mosaic.text ~style:header_style
               (pad_to_width ~width:col_w h)
           ) headers)
    in
    let rows = List.mapi (fun i (field_name, resolution) ->
        let is_cursor = i = cp.cursor_field in
        let diff = List.find_opt (fun d ->
            d.Alsdiff_merge.Xml_compare.field_name = field_name
          ) cp.field_diffs in
        let base_v = match diff with
          | Some d -> Option.value ~default:"-" d.Alsdiff_merge.Xml_compare.base_value
          | None -> "-"
        in
        let ours_v = match diff with
          | Some d -> Option.value ~default:"-" d.Alsdiff_merge.Xml_compare.ours_value
          | None -> "-"
        in
        let theirs_v = match diff with
          | Some d -> Option.value ~default:"-" d.Alsdiff_merge.Xml_compare.theirs_value
          | None -> "-"
        in
        let sel_str = match resolution with
          | Alsdiff_merge.Conflict.Ours -> "ours"
          | Theirs -> "theirs"
          | Base -> "base"
        in
        let row_style = if is_cursor
          then Mosaic.Ansi.Style.(fg Mosaic.Ansi.Color.cyan (bg Mosaic.Ansi.Color.blue default))
          else Mosaic.Ansi.Style.default
        in
        let sel_style = match resolution with
          | Alsdiff_merge.Conflict.Ours -> fg Mosaic.Ansi.Color.green row_style
          | Theirs -> fg Mosaic.Ansi.Color.red row_style
          | Base -> with_dim true row_style
        in
        let base_cell = Mosaic.text ~style:row_style
            (pad_to_width ~width:col_w (truncate_to_width ~width:col_w base_v)) in
        let ours_cell = Mosaic.text ~style:row_style
            (pad_to_width ~width:col_w (truncate_to_width ~width:col_w ours_v)) in
        let theirs_cell = Mosaic.text ~style:row_style
            (pad_to_width ~width:col_w (truncate_to_width ~width:col_w theirs_v)) in
        let sel_cell = Mosaic.text ~style:sel_style
            (pad_to_width ~width:col_w sel_str) in
        Mosaic.box ~flex_direction:Mosaic.Flex_direction.Row
          [base_cell; ours_cell; theirs_cell; sel_cell]
      ) cp.field_selections
    in
    let status = Mosaic.text ~style:(bg Mosaic.Ansi.Color.blue default)
        (pad_to_width ~width:model.viewport_width
           (Fmt.str " Tab:next a/t/b:select Enter:apply Esc:cancel | Field %d/%d "
              (cp.cursor_field + 1) (List.length cp.field_selections)))
    in
    Mosaic.box ~flex_direction:Mosaic.Flex_direction.Column
      ([title; header_row] @ rows @ [status])

let view (model : t) : Msg.t Mosaic.t =
  match model.mode with
  | Help -> render_help model
  | Detail -> render_detail model
  | Merge -> render_merge model
  | SideBySide -> render_side_by_side model
  | CherryPick -> render_cherry_pick model
