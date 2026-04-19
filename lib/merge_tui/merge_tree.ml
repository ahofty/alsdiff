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
  | Entity_conflict (c, _, _) -> Some (Filename.basename c.Conflict.path)

let all_present = { Model.ours = true; base = true; theirs = true }

let make_node ?(status = Model.Auto) ?(name = "") ?(presence = all_present)
    ?(entity_data = None)
    label ?ours_desc ?theirs_desc ?conflict_kind ?(depth = 0) children =
  { Model.path = label; label; name; presence; status; ours_desc; theirs_desc;
    conflict_kind; depth; is_expandable = children <> []; entity_data; children }

let build_xml_diff_children path ~base ~ours ~theirs =
  let diffs = Alsdiff_merge.Xml_compare.compare_three_way
      ~base ~ours ~theirs in
  List.map (fun (d : Alsdiff_merge.Xml_compare.field_diff) ->
      let ours_str = Option.value ~default:"<none>" d.ours_value in
      let theirs_str = Option.value ~default:"<none>" d.theirs_value in
      let kind_label = match d.kind with
        | Alsdiff_merge.Xml_compare.Attribute_diff -> "attr"
        | Child_element_diff -> "child"
        | Text_content_diff -> "text"
      in
      { Model.path = path ^ "/" ^ d.field_name;
        label = Fmt.str "  %s [%s]: ours=%s theirs=%s" d.field_name kind_label
            ours_str theirs_str;
        name = d.field_name;
        presence = all_present;
        status = Model.Auto;
        ours_desc = d.ours_value;
        theirs_desc = d.theirs_value;
        conflict_kind = None;
        depth = 2; is_expandable = false;
        entity_data = None; children = [] }
    ) diffs

let build_single_xml_children path (xml : Alsdiff_base.Xml.t) =
  match xml with
  | Alsdiff_base.Xml.Element { attrs; childs; _ } ->
    let attr_nodes = List.map (fun (k, v) ->
        { Model.path = path ^ "/" ^ k;
          label = Fmt.str "  %s = %s" k v;
          name = k;
          presence = all_present;
          status = Model.Auto;
          ours_desc = Some v; theirs_desc = None; conflict_kind = None;
          depth = 2; is_expandable = false;
          entity_data = None; children = [] }
      ) attrs in
    let child_counts =
      List.filter_map (function
          | Alsdiff_base.Xml.Element { name; _ } -> Some name
          | Alsdiff_base.Xml.Data _ -> None
        ) childs
      |> List.sort String.compare
      |> List.fold_left (fun acc name ->
          match acc with
          | (n, c) :: rest when String.equal n name -> (n, c + 1) :: rest
          | _ -> (name, 1) :: acc
        ) []
      |> List.rev
    in
    let child_group_nodes = List.map (fun (tag, count) ->
        { Model.path = path ^ "/" ^ tag;
          label = Fmt.str "  <%s> (%d)" tag count;
          name = tag;
          presence = all_present;
          status = Model.Auto;
          ours_desc = None; theirs_desc = None; conflict_kind = None;
          depth = 2; is_expandable = false;
          entity_data = None; children = [] }
      ) child_counts in
    attr_nodes @ child_group_nodes
  | Alsdiff_base.Xml.Data _ -> []

let rec build_entity_nodes label (merges : Patch_compare.entity_merge list) =
  let nodes = List.filter_map (fun em ->
      match em with
      | Patch_compare.Entity_keep -> None
      | Entity_add xml ->
        let name = entity_name em |> Option.value ~default:"?" in
        let children = build_single_xml_children (label ^ "/" ^ name) xml in
        Some (make_node ~name ~presence:{ Model.ours = true; base = false; theirs = false }
                ~depth:1 ~entity_data:(Some { Model.base_xml = None; ours_xml = Some xml; theirs_xml = None })
                (Fmt.str "+ %s (added)" name) children)
      | Entity_remove xml ->
        let name = entity_name em |> Option.value ~default:"?" in
        let children = build_single_xml_children (label ^ "/" ^ name) xml in
        Some (make_node ~name ~presence:{ Model.ours = false; base = true; theirs = true }
                ~depth:1 ~entity_data:(Some { Model.base_xml = Some xml; ours_xml = None; theirs_xml = None })
                (Fmt.str "- %s (removed)" name) children)
      | Entity_modify (_, xml) ->
        let name = entity_name em |> Option.value ~default:"?" in
        let children = build_single_xml_children (label ^ "/" ^ name) xml in
        Some (make_node ~name ~depth:1
                (Fmt.str "* %s (modified)" name) children)
      | Entity_modify_both (action, base_xml, ours_xml, theirs_xml) ->
        let name = entity_name em |> Option.value ~default:"Entity" in
        let field_nodes = build_recurse_fields (label ^ "/" ^ name) action in
        let edata : Model.entity_xml_data = {
          base_xml = Some base_xml; ours_xml = Some ours_xml;
          theirs_xml = Some theirs_xml
        } in
        (match action with
         | Patch_compare.Take_ours | Both_agree | Take_theirs ->
           let auto_label = match action with
             | Take_theirs -> Fmt.str "* %s (theirs)" name
             | _ -> Fmt.str "* %s (auto)" name
           in
           let path = label ^ "/" ^ name in
           let xml_children = build_xml_diff_children path
               ~base:base_xml ~ours:ours_xml ~theirs:theirs_xml in
           Some { Model.path = path;
                  label = auto_label;
                  name;
                  presence = all_present;
                  status = Model.Auto;
                  ours_desc = None; theirs_desc = None; conflict_kind = None;
                  depth = 1; is_expandable = xml_children <> [];
                  entity_data = Some edata; children = xml_children }
         | Keep ->
           None
         | Conflict c ->
           let xml_children = build_xml_diff_children c.Conflict.path
               ~base:base_xml ~ours:ours_xml ~theirs:theirs_xml in
           Some { Model.path = c.Conflict.path;
                  label = Fmt.str "! %s (conflict)" name;
                  name;
                  presence = all_present;
                  status = Model.Unresolved;
                  ours_desc = Some c.Conflict.ours_desc;
                  theirs_desc = Some c.Conflict.theirs_desc;
                  conflict_kind = Some c.Conflict.kind;
                  depth = 1; is_expandable = true;
                  entity_data = Some edata; children = xml_children }
         | Recurse _ ->
           Some (make_node ~name ~depth:1 ~entity_data:(Some edata)
                   (Fmt.str "* %s" name) field_nodes))
      | Entity_conflict (c, ours_xml, theirs_xml) ->
        let name = Filename.basename c.Conflict.path in
        let edata : Model.entity_xml_data = {
          base_xml = None; ours_xml = Some ours_xml;
          theirs_xml = Some theirs_xml
        } in
        Some { Model.path = c.Conflict.path;
               label = Fmt.str "! %s (conflict)" name;
               name;
               presence = all_present;
               status = Model.Unresolved;
               ours_desc = Some c.Conflict.ours_desc;
               theirs_desc = Some c.Conflict.theirs_desc;
               conflict_kind = Some c.Conflict.kind;
               depth = 1; is_expandable = false;
               entity_data = Some edata; children = [] }
    ) merges
  in
  if nodes = [] then [] else [ make_node ~name:label label nodes ]

and build_recurse_fields parent_path (action : Patch_compare.merge_action) =
  match action with
  | Patch_compare.Recurse fields ->
    List.filter_map (fun (f : Patch_compare.merge_field) ->
        match f.action with
        | Keep -> None
        | Take_ours ->
          Some (make_node ~name:f.field_name ~depth:2
                  (Fmt.str "* %s (ours)" f.field_name) [])
        | Take_theirs ->
          Some (make_node ~name:f.field_name ~depth:2
                  (Fmt.str "* %s (theirs)" f.field_name) [])
        | Both_agree ->
          Some (make_node ~name:f.field_name ~depth:2
                  (Fmt.str "= %s (agree)" f.field_name) [])
        | Conflict c ->
          Some { Model.path = c.Conflict.path;
                 label = Fmt.str "! %s (conflict)" f.field_name;
                 name = f.field_name;
                 presence = all_present;
                 status = Model.Unresolved;
                 ours_desc = Some c.Conflict.ours_desc;
                 theirs_desc = Some c.Conflict.theirs_desc;
                 conflict_kind = Some c.Conflict.kind;
                 depth = 2; is_expandable = false;
                 entity_data = None; children = [] }
        | Recurse sub_fields ->
          let children = build_recurse_fields
              (parent_path ^ "/" ^ f.field_name) (Recurse sub_fields) in
          Some (make_node ~name:f.field_name ~depth:2
                  (Fmt.str "* %s" f.field_name) children)
      ) fields
  | _ -> []

let build (ctx : Merge.merge_context) : Model.merge_node list =
  let track_nodes = build_entity_nodes "Tracks" ctx.track_merges in
  let return_nodes = build_entity_nodes "Returns" ctx.return_merges in
  let locator_nodes = build_entity_nodes "Locators" ctx.locator_merges in
  let top_action_nodes = build_recurse_fields "" ctx.action in
  top_action_nodes @ track_nodes @ return_nodes @ locator_nodes
