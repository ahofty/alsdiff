open Model
open Mosaic.Ansi.Style

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
  let help_text = "↑↓:nav Space:expand d:mode /:search q:quit" in
  let status = Fmt.str " Mode: %s | %s | %s | %s " mode_name filter_text search_text help_text in
  let bar_style = bg Mosaic.Ansi.Color.blue default in
  Mosaic.text ~style:bar_style status

let view (model : Model.t) : Msg.t Mosaic.t =
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
    let rec skip n = function [] -> [] | l when n <= 0 -> l | _ :: t -> skip (n - 1) t in
    skip scroll_offset visible_nodes
    |> fun l ->
    let rec take n = function [] -> [] | _ when n <= 0 -> [] | x :: t -> x :: take (n - 1) t in
    take max_rows l
  in
  let nodes_box = List.mapi (fun i node ->
      let global_i = scroll_offset + i in
      render_node ~model ~is_cursor:(global_i = model.cursor_index) node
    ) visible_slice
  in
  Mosaic.box ~flex_direction:Mosaic.Flex_direction.Column [
    Mosaic.box ~flex_direction:Mosaic.Flex_direction.Column ~flex_grow:1. nodes_box;
    render_status_bar model;
  ]
