open Alsdiff_merge

let xml_attr xml attr =
  match xml with
  | Alsdiff_base.Xml.Element { attrs; _ } ->
    (match List.find_opt (fun (k, _) -> String.equal k attr) attrs with
     | Some (_, v) -> Some v
     | None -> None)
  | _ -> None

let entity_name em =
  let get_name xml =
    match xml_attr xml "Name", xml_attr xml "Id" with
    | Some n, _ -> n
    | _, Some id -> "Entity(" ^ id ^ ")"
    | _ -> "Unknown"
  in
  match em with
  | Patch_compare.Entity_keep -> None
  | Entity_add xml -> Some (get_name xml)
  | Entity_remove xml -> Some (get_name xml)
  | Entity_modify (_, xml) -> Some (get_name xml)
  | Entity_modify_both (_, _, ours, _) ->
    (match xml_attr ours "Name" with
     | Some n -> Some n
     | None -> Some "Entity")
  | Entity_conflict c -> Some (Filename.basename c.Conflict.path)

let make_node ?(status = Model.Auto) label ?ours_desc ?theirs_desc
    ?(depth = 0) children =
  { Model.path = label; label; status; ours_desc; theirs_desc;
    depth; is_expandable = children <> []; children }

let rec build_entity_nodes label (merges : Patch_compare.entity_merge list) =
  let nodes = List.filter_map (fun em ->
      match em with
      | Patch_compare.Entity_keep -> None
      | Entity_add _ ->
        let name = entity_name em |> Option.value ~default:"?" in
        Some (make_node ~depth:1
                (Fmt.str "+ %s (added)" name) [])
      | Entity_remove _ ->
        let name = entity_name em |> Option.value ~default:"?" in
        Some (make_node ~depth:1
                (Fmt.str "- %s (removed)" name) [])
      | Entity_modify (_, _) ->
        let name = entity_name em |> Option.value ~default:"?" in
        Some (make_node ~depth:1
                (Fmt.str "* %s (modified)" name) [])
      | Entity_modify_both (action, _, _, _) ->
        let name = entity_name em |> Option.value ~default:"Entity" in
        let field_nodes = build_recurse_fields (label ^ "/" ^ name) action in
        (match action with
         | Patch_compare.Take_ours | Both_agree ->
           Some (make_node ~depth:1
                   (Fmt.str "* %s (auto)" name) field_nodes)
         | Take_theirs ->
           Some (make_node ~depth:1
                   (Fmt.str "* %s (theirs)" name) field_nodes)
         | Keep ->
           None
         | Conflict c ->
           Some { Model.path = c.Conflict.path;
                  label = Fmt.str "! %s (conflict)" name;
                  status = Model.Unresolved;
                  ours_desc = Some c.Conflict.ours_desc;
                  theirs_desc = Some c.Conflict.theirs_desc;
                  depth = 1; is_expandable = false; children = [] }
         | Recurse _ ->
           Some (make_node ~depth:1
                   (Fmt.str "* %s" name) field_nodes))
      | Entity_conflict c ->
        Some { Model.path = c.Conflict.path;
               label = Fmt.str "! %s (conflict)" (Filename.basename c.Conflict.path);
               status = Model.Unresolved;
               ours_desc = Some c.Conflict.ours_desc;
               theirs_desc = Some c.Conflict.theirs_desc;
               depth = 1; is_expandable = false; children = [] }
    ) merges
  in
  if nodes = [] then [] else [ make_node label nodes ]

and build_recurse_fields parent_path (action : Patch_compare.merge_action) =
  match action with
  | Patch_compare.Recurse fields ->
    List.filter_map (fun (f : Patch_compare.merge_field) ->
        match f.action with
        | Keep -> None
        | Take_ours ->
          Some (make_node ~depth:2
                  (Fmt.str "* %s (ours)" f.field_name) [])
        | Take_theirs ->
          Some (make_node ~depth:2
                  (Fmt.str "* %s (theirs)" f.field_name) [])
        | Both_agree ->
          Some (make_node ~depth:2
                  (Fmt.str "= %s (agree)" f.field_name) [])
        | Conflict c ->
          Some { Model.path = c.Conflict.path;
                 label = Fmt.str "! %s (conflict)" f.field_name;
                 status = Model.Unresolved;
                 ours_desc = Some c.Conflict.ours_desc;
                 theirs_desc = Some c.Conflict.theirs_desc;
                 depth = 2; is_expandable = false; children = [] }
        | Recurse sub_fields ->
          let children = build_recurse_fields
              (parent_path ^ "/" ^ f.field_name) (Recurse sub_fields) in
          Some (make_node ~depth:2
                  (Fmt.str "* %s" f.field_name) children)
      ) fields
  | _ -> []

let build (ctx : Merge.merge_context) : Model.merge_node list =
  let track_nodes = build_entity_nodes "Tracks" ctx.track_merges in
  let return_nodes = build_entity_nodes "Returns" ctx.return_merges in
  let locator_nodes = build_entity_nodes "Locators" ctx.locator_merges in
  let top_action_nodes = build_recurse_fields "" ctx.action in
  top_action_nodes @ track_nodes @ return_nodes @ locator_nodes
