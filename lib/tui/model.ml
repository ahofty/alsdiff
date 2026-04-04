open Alsdiff_output
open View_model

module StringSet = Set.Make (String)

type t = {
  views : view list;
  config : Config.detail_config;
  cursor_path : string list;
  expanded_paths : StringSet.t;
  search_query : string option;
  search_mode : bool;
  filter_change : View_model.change_type option;
  detail_modes : Config.detail_config list;
  detail_mode_index : int;
  (* Track flat list of all nodes for navigation *)
  flat_nodes : tree_node list;
  cursor_index : int;
  viewport_height : int;
}

and tree_node = {
  path : string list;
  label : string;
  change : View_model.change_type;
  depth : int;
  is_expandable : bool;
  children : tree_node list;
}

(* Format field value for display *)
let format_field_value = function
  | View_model.Fint i -> Fmt.str "%d" i
  | View_model.Ffloat f -> Fmt.str "%.2f" f
  | View_model.Fbool b -> Fmt.str "%b" b
  | View_model.Fstring s -> s

(* Format field label as "name: old -> new" *)
let format_field_label (field : View_model.field) : string =
  let value_part = match field.oldval, field.newval with
    | Some old_v, Some new_v ->
      Fmt.str "%s: %s -> %s" field.name (format_field_value old_v) (format_field_value new_v)
    | Some old_v, None ->
      Fmt.str "%s: %s" field.name (format_field_value old_v)
    | None, Some new_v ->
      Fmt.str "%s: %s" field.name (format_field_value new_v)
    | None, None -> field.name
  in
  value_part

let make_detail_modes () = [
  Config.quiet;
  Config.compact;
  Config.inline;
  Config.full;
  Config.verbose;
]

let rec build_nodes_with_config
    ~(cfg : Config.detail_config)
    ?(path = [])
    ?(depth = 0)
    (views : view list)
  : tree_node list =
  List.concat (List.map (fun (v : view) ->
      match v with
      | Field f ->
        let field_level = Config.get_effective_detail cfg f.change f.domain_type in
        if Config.should_render_level field_level then begin
          (* Only show fields at Inline or Full detail levels *)
          if field_level = Config.Full || field_level = Config.Inline then begin
            let node_path = path @ [f.name] in
            (* Format field label with values *)
            let label = format_field_label f in
            [{
              path = node_path; label;
              change = f.change; depth;
              is_expandable = false; children = []
            }]
          end else []
        end else []
      | Item i ->
        let item_level = Config.get_effective_detail cfg i.change i.domain_type in
        if Config.should_render_level item_level then begin
          let node_path = path @ [i.name] in
          let children =
            build_nodes_with_config ~cfg ~path:node_path
              ~depth:(depth + 1) i.children
          in
          [{
            path = node_path; label = i.name;
            change = i.change; depth;
            is_expandable = children <> []; children
          }]
        end else []
      | Collection c ->
        let col_level = Config.get_effective_detail cfg c.change c.domain_type in
        if Config.should_render_level col_level then begin
          let node_path = path @ [c.name] in
          (* Filter collection items based on config *)
          let filtered_items = Config.filter_collection_elements cfg c in
          (* Convert filtered items back to views *)
          let filtered_views = List.map (fun (item : item) -> Item item) filtered_items in
          let children =
            build_nodes_with_config ~cfg ~path:node_path
              ~depth:(depth + 1) filtered_views
          in
          [{
            path = node_path; label = c.name;
            change = c.change; depth;
            is_expandable = children <> []; children
          }]
        end else []
    ) views)

let init ?(detail_config = Config.compact) (views : view list) : t =
  let detail_modes = make_detail_modes () in
  let detail_mode_index =
    match List.find_index (fun cfg -> cfg = detail_config) detail_modes with
    | Some idx -> idx
    | None -> 1
  in
  let flat_nodes = build_nodes_with_config ~cfg:detail_config views in
  {
    views;
    config = detail_config;
    cursor_path = [];
    expanded_paths = StringSet.empty;
    search_query = None;
    search_mode = false;
    filter_change = None;
    detail_modes;
    detail_mode_index;
    flat_nodes;
    cursor_index = 0;
    viewport_height = 24;
  }
