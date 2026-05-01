open Alsdiff_output
open View_model

module StringSet = Set.Make (String)

type mode = Browser | Diff | Help | Stats

type export_format_option = Text | Json | Statistics

(* Centralized list of export format options *)
let export_formats = [Text; Json; Statistics]

type file_entry = {
  name : string;
  path : string;
  is_dir : bool;
}

type t = {
  mode : mode;
  previous_mode : mode option;
  views : view list;
  config : Config.detail_config;
  expanded_paths : StringSet.t;
  last_error : string option;
  search_query : string option;
  search_mode : bool;
  filter_change : View_model.change_type option;
  detail_modes : Config.detail_config list;
  detail_mode_index : int;
  (* Track flat list of all nodes for navigation *)
  flat_nodes : tree_node list;
  cursor_index : int;
  viewport_width : int;
  viewport_height : int;
  (* Browser state *)
  browser_root : string;
  browser_cwd : string;
  browser_entries : file_entry list;
  browser_cursor : int;
  browser_selected : string list;
  (* Navigation history *)
  nav_back : string list list;
  nav_forward : string list list;
  (* Export selector state *)
  export_selector_active : bool;
  export_selected_format : export_format_option;
  (* Focus mode - show only descendants of focused node *)
  focused_path : string list option;
  note_name_style : View_model.note_display_style;
  time_format : View_model.time_format;
  format_time : (float -> View_model.field_value) option;
}

and tree_node = {
  path : string list;
  label : string;
  change : View_model.change_type;
  depth : int;
  is_expandable : bool;
  children : tree_node list;
}

type change_stats = {
  added : int;
  removed : int;
  modified : int;
  unchanged : int;
  total : int;
}

type domain_stats = {
  name : string;
  changes : change_stats;
}

let compute_stats (nodes : tree_node list) : change_stats * domain_stats list =
  let rec count_nodes acc = function
    | [] -> acc
    | node :: rest ->
      let new_acc = match node.change with
        | View_model.Added -> { acc with added = acc.added + 1; total = acc.total + 1 }
        | View_model.Removed -> { acc with removed = acc.removed + 1; total = acc.total + 1 }
        | View_model.Modified -> { acc with modified = acc.modified + 1; total = acc.total + 1 }
        | View_model.Unchanged -> { acc with unchanged = acc.unchanged + 1; total = acc.total + 1 }
      in
      count_nodes (count_nodes new_acc node.children) rest
  in
  let total_stats = count_nodes { added = 0; removed = 0; modified = 0; unchanged = 0; total = 0 } nodes in
  (* Group by top-level domain (first path element) *)
  let group_by_domain nodes =
    let group_helper acc nodes =
      List.fold_left (fun acc' node ->
          let domain = match node.path with | [] -> "Root" | d :: _ -> d in
          let stats = try List.assoc domain acc' with Not_found -> { added = 0; removed = 0; modified = 0; unchanged = 0; total = 0 } in
          let node_stats = match node.change with
            | View_model.Added -> { stats with added = stats.added + 1; total = stats.total + 1 }
            | View_model.Removed -> { stats with removed = stats.removed + 1; total = stats.total + 1 }
            | View_model.Modified -> { stats with modified = stats.modified + 1; total = stats.total + 1 }
            | View_model.Unchanged -> { stats with unchanged = stats.unchanged + 1; total = stats.total + 1 }
          in
          List.remove_assoc domain acc' @ [(domain, node_stats)]
        ) acc nodes
    in
    group_helper [] nodes
  in
  let domain_list = List.map (fun (name, changes) -> { name; changes }) (group_by_domain nodes) in
  (total_stats, domain_list)

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
  Config.composer;
  Config.mixing;
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

let read_dir_entries ~root:_ ~cwd : file_entry list =
  let entries =
    try
      let ds = Unix.opendir cwd in
      let rec loop acc =
        match Unix.readdir ds with
        | exception End_of_file -> Unix.closedir ds; acc
        | name ->
          if name = "." then loop acc
          else if name = ".." then loop acc
          else begin
            let full_path = Filename.concat cwd name in
            let is_dir =
              try Unix.(stat full_path).st_kind = S_DIR
              with Unix.Unix_error _ -> false
            in
            let is_als = Filename.check_suffix name ".als" in
            if is_dir || is_als then
              loop ({ name; path = full_path; is_dir } :: acc)
            else loop acc
          end
      in
      loop []
    with Unix.Unix_error _ -> []
  in
  let sort_key (e : file_entry) = (not e.is_dir, String.lowercase_ascii e.name) in
  List.sort (fun a b -> compare (sort_key a) (sort_key b)) entries

let init_browser ~root ?(note_name_style = View_model.Sharp)
    ?(time_format = View_model.QuarterNotes) () : t =
  let cwd = root in
  let entries = read_dir_entries ~root ~cwd in
  let detail_modes = make_detail_modes () in
  let detail_mode_index =
    match List.find_index (fun cfg -> cfg = Config.full) detail_modes with
    | Some idx -> idx
    | None -> 1
  in
  {
    mode = Browser;
    previous_mode = None;
    views = [];
    config = Config.full;
    expanded_paths = StringSet.empty;
    search_query = None;
    search_mode = false;
    filter_change = None;
    detail_modes;
    detail_mode_index;
    flat_nodes = [];
    cursor_index = 0;
    viewport_width = 80;
    viewport_height = 24;
    last_error = None;
    browser_root = root;
    browser_cwd = cwd;
    browser_entries = entries;
    browser_cursor = 0;
    browser_selected = [];
    nav_back = [];
    nav_forward = [];
    export_selector_active = false;
    export_selected_format = Text;
    focused_path = None;
    note_name_style;
    time_format;
    format_time = None;
  }

let init ?(detail_config = Config.compact) ?(note_name_style = View_model.Sharp)
    ?(time_format = View_model.QuarterNotes) ?(format_time = None) (views : view list) : t =
  let detail_modes = make_detail_modes () in
  let detail_mode_index =
    match List.find_index (fun cfg -> cfg = detail_config) detail_modes with
    | Some idx -> idx
    | None -> 1
  in
  let flat_nodes = build_nodes_with_config ~cfg:detail_config views in
  {
    mode = Diff;
    previous_mode = None;
    views;
    config = detail_config;
    expanded_paths = StringSet.empty;
    search_query = None;
    search_mode = false;
    filter_change = None;
    detail_modes;
    detail_mode_index;
    flat_nodes;
    cursor_index = 0;
    viewport_width = 80;
    viewport_height = 24;
    last_error = None;
    browser_root = "";
    browser_cwd = "";
    browser_entries = [];
    browser_cursor = 0;
    browser_selected = [];
    nav_back = [];
    nav_forward = [];
    export_selector_active = false;
    export_selected_format = Text;
    focused_path = None;
    note_name_style;
    time_format;
    format_time;
  }
