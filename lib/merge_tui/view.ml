open Model

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
      " %d/%d resolved | %d conflicts | j/k:nav Tab:next a/t/b:resolve w:write q:quit "
      resolved total conflicts
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
        ("Space / Enter", "Toggle expand");
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

let view (model : t) : Msg.t Mosaic.t =
  match model.mode with
  | Help -> render_help model
  | Merge -> render_merge model
