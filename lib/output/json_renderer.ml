open Output_types
open Presentation_model
open Config

let field_value_to_yojson (v : field_value) : Yojson.Safe.t =
  match v with
  | Fint i -> `Int i
  | Ffloat f -> `Float f
  | Fbool b -> `Bool b
  | Fstring s -> `String s

let change_type_to_string = function
  | Unchanged -> "Unchanged"
  | Added -> "Added"
  | Removed -> "Removed"
  | Modified -> "Modified"


let rec item_to_yojson (cfg : detail_config) (item : item) : Yojson.Safe.t option =
  let level = get_effective_detail cfg item.change item.domain_type in
  (* Always render Unchanged items that have children (populated from reference) *)
  if not (should_render_level level) && not (item.change = Unchanged && item.children <> []) then None
  else
    let children =
      List.filter_map (view_to_yojson cfg) item.children
    in
    let base = [
      ("type", `String "item");
      ("name", `String item.name);
      ("change", `String (change_type_to_string item.change));
      ("domain_type", `String (domain_type_to_string item.domain_type));
    ] in
    let with_children =
      if children = [] then base
      else base @ [("children", `List children)]
    in
    Some (`Assoc with_children)

and collection_to_yojson (cfg : detail_config) (col : collection) : Yojson.Safe.t option =
  let filtered, truncation_info = filter_collection_elements_with_info cfg col in
  if filtered = [] then None
  else
    let items = List.filter_map (item_to_yojson cfg) filtered in
    let base = [
      ("type", `String "collection");
      ("name", `String col.name);
      ("change", `String (change_type_to_string col.change));
      ("domain_type", `String (domain_type_to_string col.domain_type));
    ] in
    let with_items = base @ [("items", `List items)] in
    let with_truncation = match truncation_info with
      | None -> with_items
      | Some info ->
        let breakdown = info.truncated_breakdown in
        let truncated_obj = [
          ("added", `Int breakdown.added);
          ("removed", `Int breakdown.removed);
          ("modified", `Int breakdown.modified);
        ] in
        with_items @ [
          ("total", `Int info.total);
          ("displayed", `Int info.displayed);
          ("truncated", `Assoc truncated_obj);
        ]
    in
    Some (`Assoc with_truncation)

and view_to_yojson (cfg : detail_config) (view : view) : Yojson.Safe.t option =
  match view with
  | Field f ->
    let level = get_effective_detail cfg f.change f.domain_type in
    if not (should_render_level level) then None
    else
      let base = [
        ("type", `String "field");
        ("name", `String f.name);
        ("change", `String (change_type_to_string f.change));
        ("domain_type", `String (domain_type_to_string f.domain_type));
      ] in
      let with_old = match f.oldval with
        | None -> base
        | Some v -> base @ [("old_value", field_value_to_yojson v)]
      in
      let with_new = match f.newval with
        | None -> with_old
        | Some v -> with_old @ [("new_value", field_value_to_yojson v)]
      in
      Some (`Assoc with_new)
  | Item item -> item_to_yojson cfg item
  | Collection col -> collection_to_yojson cfg col

let render (cfg : detail_config) (views : view list) : string =
  let entries = List.filter_map (view_to_yojson cfg) views in
  let json = `Assoc [("diff", `List entries)] in
  Yojson.Safe.pretty_to_string json
