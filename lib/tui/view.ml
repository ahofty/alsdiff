open Model
open Mosaic.Ansi.Style

let render_browser_entry
    ~(is_cursor : bool) (is_selected : bool) (entry : Model.file_entry) : Msg.t Mosaic.t =
  let icon = if entry.is_dir then " " else "  " in
  let marker = if is_selected then " [x]" else "" in
  let cursor_str = if is_cursor then " > " else "   " in
  let text = Fmt.str "%s%s%s%s" cursor_str icon entry.name marker in
  let style =
    let base = if entry.is_dir
      then fg Mosaic.Ansi.Color.blue default
      else default
    in
    if is_cursor then base ++ bg Mosaic.Ansi.Color.cyan default
    else if is_selected then base ++ fg Mosaic.Ansi.Color.green default
    else base
  in
  Mosaic.text ~style text

let render_browser_status_bar (model : Model.t) : Msg.t Mosaic.t =
  let selected_text = match model.browser_selected with
    | [] -> ""
    | [f1] ->
      let name = Filename.basename f1 in
      Fmt.str " [1] %s" name
    | [f1; f2] ->
      let n1 = Filename.basename f1 in
      let n2 = Filename.basename f2 in
      Fmt.str " [1] %s [2] %s" n1 n2
    | _ -> ""
  in
  let error_text = match model.last_error with
    | Some e -> Fmt.str " ERROR: %s " e
    | None -> ""
  in
  let help = "Enter:select Esc:back q:quit" in
  let status = Fmt.str " %s | %s%s " selected_text help error_text in
  let bar_style = match model.last_error with
    | Some _ -> bg Mosaic.Ansi.Color.red default
    | None -> bg Mosaic.Ansi.Color.blue default
  in
  Mosaic.text ~style:bar_style status

let view_browser (model : Model.t) : Msg.t Mosaic.t =
  let total = List.length model.browser_entries in
  let max_rows = max 1 (model.viewport_height - 2) in
  let scroll_offset =
    if total <= max_rows then 0
    else
      let half = max_rows / 2 in
      max 0 (min (model.browser_cursor - half) (total - max_rows))
  in
  let visible_slice =
    List.drop scroll_offset model.browser_entries
    |> List.take max_rows
  in
  let header_style = fg Mosaic.Ansi.Color.yellow default in
  let header = Mosaic.text ~style:header_style (Fmt.str " %s" model.browser_cwd) in
  let entries_box = List.mapi (fun i (entry : Model.file_entry) ->
      let global_i = scroll_offset + i in
      let is_cursor = global_i = model.browser_cursor in
      let is_selected = Stdlib.List.mem entry.path model.browser_selected in
      render_browser_entry ~is_cursor is_selected entry
    ) visible_slice
  in
  Mosaic.box ~flex_direction:Mosaic.Flex_direction.Column [
    header;
    Mosaic.box ~flex_direction:Mosaic.Flex_direction.Column ~flex_grow:1. entries_box;
    render_browser_status_bar model;
  ]

let render_node ~(model : Model.t) ~(is_cursor : bool) (node : Model.tree_node) : Msg.t Mosaic.t =
  let prefix = String.make (node.depth * 2) ' ' in
  let symbol = match node.change with
    | Alsdiff_output.View_model.Added -> "+"
    | Alsdiff_output.View_model.Removed -> "-"
    | Alsdiff_output.View_model.Modified -> "*"
    | Alsdiff_output.View_model.Unchanged -> "="
  in
  let path_key = String.concat "/" node.path in
  let is_expanded = StringSet.mem path_key model.expanded_paths in
  let expand_indicator = if node.is_expandable then (if is_expanded then "▼" else "▶") else " " in
  let cursor_str = if is_cursor then "► " else "  " in
  let text = Fmt.str "%s%s %s %s%s" cursor_str prefix expand_indicator symbol node.label in
  let change_style = match node.change with
    | Alsdiff_output.View_model.Added ->
      fg Mosaic.Ansi.Color.green default
    | Alsdiff_output.View_model.Removed ->
      fg Mosaic.Ansi.Color.red default
    | Alsdiff_output.View_model.Modified ->
      fg Mosaic.Ansi.Color.yellow default
    | Alsdiff_output.View_model.Unchanged ->
      with_dim true default
  in
  let style = if is_cursor
    then change_style ++ fg Mosaic.Ansi.Color.cyan default
    else change_style
  in
  Mosaic.text ~style text

let render_status_bar (model : Model.t) : Msg.t Mosaic.t =
  let mode_name = match model.detail_mode_index with
    | 0 -> "quiet"
    | 1 -> "compact"
    | 2 -> "inline"
    | 3 -> "full"
    | 4 -> "verbose"
    | _ -> "unknown"
  in
  let filter_text = match model.filter_change with
    | Some Alsdiff_output.View_model.Added -> "Filter: Added"
    | Some Alsdiff_output.View_model.Removed -> "Filter: Removed"
    | Some Alsdiff_output.View_model.Modified -> "Filter: Modified"
    | Some Alsdiff_output.View_model.Unchanged -> "Filter: Unchanged"
    | None -> "Filter: None"
  in
  let search_text = if model.search_mode then
      Fmt.str "SEARCH: %s" (match model.search_query with Some q -> q | None -> "")
    else match model.search_query with
      | Some q -> Fmt.str "Search: %s" q
      | None -> ""
  in
  let focus_text = match model.focused_path with
    | Some path -> Fmt.str "[FOCUS: %s]" (String.concat "/" path)
    | None -> ""
  in
  let help_text = "↑↓:nav Space:expand d:mode /:search z:focus q:quit" in
  let parts = [
    Fmt.str "Mode: %s" mode_name;
    filter_text;
    search_text;
    focus_text;
    help_text;
  ] in
  let non_empty_parts = List.filter (fun s -> s <> "") parts in
  let status = String.concat " | " non_empty_parts in
  let bar_style = bg Mosaic.Ansi.Color.blue default in
  Mosaic.text ~style:bar_style status

let render_help (model : Model.t) : Msg.t Mosaic.t =
  let title_style = fg Mosaic.Ansi.Color.yellow default in
  let mode_name = match model.mode with
    | Model.Browser -> "File Browser"
    | Model.Diff -> "Diff View"
    | Model.Help -> "Help"
    | Model.Stats -> "Statistics"
  in
  let title = Mosaic.text ~style:title_style (Fmt.str "alsdiff_tui - Help (%s)" mode_name) in
  let section_style = fg Mosaic.Ansi.Color.cyan default in
  let key_style = fg Mosaic.Ansi.Color.green default in

  (* Mode-specific help sections *)
  let diff_sections = [
    ("Navigation", [
        ("↑ / k", "Move up");
        ("↓ / j", "Move down");
        ("← / h", "Collapse / Go left");
        ("→ / l", "Expand / Go right");
        ("PageUp / PageDown", "Page up/down");
        ("Home / End", "Jump to start/end");
        ("[", "Navigate back");
        ("]", "Navigate forward");
      ]);
    ("Actions", [
        ("Space / Enter", "Toggle expand");
        ("d", "Cycle detail mode");
        ("f", "Cycle change filter");
        ("/", "Start search");
        ("z", "Enter focus mode");
        ("?", "Show this help");
        ("s", "Show statistics");
        ("E", "Export (choose format)");
        ("q", "Quit / Back to browser");
      ]);
    ("Search", [
        ("Escape", "Clear search");
        ("Enter", "End search");
        ("Backspace", "Delete character");
      ]);
    ("Focus Mode", [
        ("z", "Enter focus on current node");
        ("Esc", "Exit focus mode");
      ]);
  ] in

  let browser_sections = [
    ("Navigation", [
        ("↑ / k / p", "Move up");
        ("↓ / j / n", "Move down");
        ("PageUp / PageDown", "Page up/down");
        ("Home / End", "Jump to start/end");
      ]);
    ("Actions", [
        ("Enter", "Enter directory / Select file");
        ("Esc", "Go up one directory");
        ("?", "Show this help");
        ("q", "Quit");
      ]);
    ("File Selection", [
        ("Select up to 2", ".als files to compare");
        ("Selected files", "shown in status bar");
        ("Re-select file", "to deselect it");
      ]);
  ] in

  let help_sections = match model.mode with
    | Model.Browser -> browser_sections
    | Model.Diff | Model.Help | Model.Stats -> diff_sections
  in

  let render_section (section_name, bindings) =
    let header = Mosaic.text ~style:section_style (Fmt.str "\n%s:" section_name) in
    let bindings_list = List.map (fun (key, desc) ->
        let key_text = Mosaic.text ~style:key_style (Fmt.str "  %-18s" key) in
        let desc_text = Mosaic.text desc in
        Mosaic.box ~flex_direction:Mosaic.Flex_direction.Row [key_text; desc_text]
      ) bindings in
    Mosaic.box ~flex_direction:Mosaic.Flex_direction.Column (header :: bindings_list)
  in
  let sections = List.map render_section help_sections in
  let footer = Mosaic.text ~style:(fg Mosaic.Ansi.Color.blue default) "\nPress any key to close" in
  Mosaic.box ~flex_direction:Mosaic.Flex_direction.Column (title :: sections @ [footer])

let render_export_selector (model : Model.t) : Msg.t Mosaic.t =
  let title_style = fg Mosaic.Ansi.Color.yellow default in
  let title = Mosaic.text ~style:title_style "Export Format:" in

  let format_descriptions = [
    (Text, "Text - Plain text hierarchical view");
    (Json, "JSON - Structured data format");
    (Statistics, "Statistics - Summary of changes");
  ] in
  let options = format_descriptions in

  let render_option (format, desc) =
    let is_selected = model.export_selected_format = format in
    let cursor = if is_selected then "► " else "  " in
    let style = if is_selected
      then fg Mosaic.Ansi.Color.cyan default
      else default
    in
    Mosaic.text ~style (Fmt.str "%s%s" cursor desc)
  in

  let option_list = List.map render_option options in
  let footer_style = fg Mosaic.Ansi.Color.blue default in
  let footer = Mosaic.text ~style:footer_style
      "↑↓: Navigate | Enter: Export | Esc: Cancel" in

  (* Add padding above and below for visual centering *)
  let padding_top = List.init 6 (fun _ -> Mosaic.text "") in
  let padding_bottom = List.init 6 (fun _ -> Mosaic.text "") in
  Mosaic.box ~flex_direction:Mosaic.Flex_direction.Column
    (padding_top @ title :: option_list @ [footer] @ padding_bottom)

let render_stats (model : Model.t) : Msg.t Mosaic.t =
  let title_style = fg Mosaic.Ansi.Color.yellow default in
  let title = Mosaic.text ~style:title_style "alsdiff_tui - Statistics" in
  let (total_stats, domain_stats) = Model.compute_stats model.flat_nodes in
  let num_style = fg Mosaic.Ansi.Color.green default in
  let label_style = fg Mosaic.Ansi.Color.cyan default in
  let render_stat_line label value =
    Mosaic.box ~flex_direction:Mosaic.Flex_direction.Row [
      Mosaic.text ~style:label_style (Fmt.str "  %-20s" label);
      Mosaic.text ~style:num_style (Fmt.str "%d" value);
    ]
  in
  let render_change_stats (stats : Model.change_stats) =
    [
      render_stat_line "Total changes:" stats.total;
      render_stat_line "Added:" stats.added;
      render_stat_line "Removed:" stats.removed;
      render_stat_line "Modified:" stats.modified;
      render_stat_line "Unchanged:" stats.unchanged;
    ]
  in
  let total_section = Mosaic.text ~style:(fg Mosaic.Ansi.Color.cyan default) "\nTotal:" in
  let total_lines = render_change_stats total_stats in
  let domain_section = Mosaic.text ~style:(fg Mosaic.Ansi.Color.cyan default) "\nBy Domain:" in
  let render_domain (d : Model.domain_stats) =
    let header = Mosaic.text ~style:(fg Mosaic.Ansi.Color.magenta default) (Fmt.str "\n  %s:" d.name) in
    let lines = render_change_stats d.changes in
    Mosaic.box ~flex_direction:Mosaic.Flex_direction.Column (header :: lines)
  in
  let domain_lines = List.map render_domain domain_stats in
  let footer = Mosaic.text ~style:(fg Mosaic.Ansi.Color.blue default) "\nPress any key to close" in
  Mosaic.box ~flex_direction:Mosaic.Flex_direction.Column
    ([title; total_section] @ total_lines @ [domain_section] @ domain_lines @ [footer])

let view (model : Model.t) : Msg.t Mosaic.t =
  match model.mode with
  | Model.Help -> render_help model
  | Model.Stats -> render_stats model
  | Model.Browser -> view_browser model
  | Model.Diff ->
    if model.export_selector_active then
      (* Show export selector as full view *)
      render_export_selector model
    else
      (* Normal diff view *)
      let visible_nodes = Update.get_visible_nodes model in
      let total = List.length visible_nodes in
      let max_rows = max 1 (model.viewport_height - 1) in
      (* Calculate scroll window to keep cursor visible *)
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
      (* Add blank lines to fill the viewport and keep status bar at bottom *)
      let num_blank_rows = max_rows - List.length visible_slice in
      let blank_lines = if num_blank_rows > 0
        then List.map (fun _ -> Mosaic.text "") (Array.to_list (Array.make num_blank_rows ()))
        else []
      in
      let all_rows = nodes_box @ blank_lines in
      Mosaic.box ~flex_direction:Mosaic.Flex_direction.Column [
        Mosaic.box ~flex_direction:Mosaic.Flex_direction.Column all_rows;
        render_status_bar model;
      ]
