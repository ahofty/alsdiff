open Alsdiff_base

let rec resolve_action_tree
    (resolutions : (string, Conflict.resolution) Hashtbl.t)
    (action : Patch_compare.merge_action) : Patch_compare.merge_action =
  match action with
  | Patch_compare.Recurse fields ->
    Patch_compare.Recurse (List.map (fun f ->
        { f with Patch_compare.action = resolve_action_tree resolutions f.Patch_compare.action }
      ) fields)
  | Patch_compare.Conflict c ->
    (match Hashtbl.find_opt resolutions c.Conflict.path with
     | Some Conflict.Ours -> Patch_compare.Take_ours
     | Some Conflict.Theirs -> Patch_compare.Take_theirs
     | Some Conflict.Base -> Patch_compare.Keep
     | None -> Patch_compare.Take_ours)
  | other -> other

let replace_attr (xml : Xml.t) (attr_name : string) (value : string) : Xml.t =
  match xml with
  | Xml.Element { name; attrs; childs } ->
    let attrs = List.map (fun (k, v) ->
        if String.equal k attr_name then (k, value) else (k, v)
      ) attrs
    in
    Xml.Element { name; attrs; childs }
  | Xml.Data _ -> xml

let find_child_by_tag (xml : Xml.t) (tag : string) : Xml.t option =
  match xml with
  | Xml.Element { childs; _ } ->
    List.find_opt (fun c ->
        match c with
        | Xml.Element { name; _ } -> String.equal name tag
        | Xml.Data _ -> false
      ) childs
  | Xml.Data _ -> None

let replace_child_by_tag (xml : Xml.t) (tag : string) (replacement : Xml.t) : Xml.t =
  match xml with
  | Xml.Element { name; attrs; childs } ->
    let childs = List.map (fun c ->
        match c with
        | Xml.Element { name = n; _ } when String.equal n tag -> replacement
        | _ -> c
      ) childs
    in
    Xml.Element { name; attrs; childs }
  | Xml.Data _ -> xml

let remove_child_by_tag (xml : Xml.t) (tag : string) : Xml.t =
  match xml with
  | Xml.Element { name; attrs; childs } ->
    let childs = List.filter (fun c ->
        match c with
        | Xml.Element { name = n; _ } -> not (String.equal n tag)
        | _ -> true
      ) childs
    in
    Xml.Element { name; attrs; childs }
  | Xml.Data _ -> xml

let add_child_if_missing (xml : Xml.t) (tag : string) (child : Xml.t) : Xml.t =
  match find_child_by_tag xml tag with
  | Some _ -> xml
  | None -> Xml.add_child xml child

let splice_field (template : Xml.t) (source : Xml.t)
    (loc : Patch_compare.xml_location) : Xml.t =
  match loc with
  | Patch_compare.Attr attr_name ->
    (match Xml.get_attr_opt attr_name source with
     | Some value -> replace_attr template attr_name value
     | None -> template)
  | Patch_compare.Child tag ->
    (match find_child_by_tag source tag with
     | Some child ->
       (match find_child_by_tag template tag with
        | Some _ -> replace_child_by_tag template tag child
        | None -> Xml.add_child template child)
     | None -> remove_child_by_tag template tag)
  | Patch_compare.Path _ ->
    (* Path-based splicing requires Upath support; fall back to ours *)
    source

let merge_from_fields
    ~(base : Xml.t) ~(ours : Xml.t) ~(theirs : Xml.t)
    ~(fields : Patch_compare.merge_field list)
    ~(resolutions : (string, Conflict.resolution) Hashtbl.t) : Xml.t =
  let xml = ref base in
  List.iter (fun (f : Patch_compare.merge_field) ->
      let resolved_action = resolve_action_tree resolutions f.action in
      match resolved_action with
      | Patch_compare.Keep -> ()
      | Take_ours ->
        (match f.xml_loc with
         | Some loc -> xml := splice_field !xml ours loc
         | None -> xml := ours)
      | Take_theirs ->
        (match f.xml_loc with
         | Some loc -> xml := splice_field !xml theirs loc
         | None -> xml := theirs)
      | Both_agree ->
        (match f.xml_loc with
         | Some loc -> xml := splice_field !xml ours loc
         | None -> ())
      | Recurse _ ->
        (match f.xml_loc with
         | Some loc -> xml := splice_field !xml ours loc
         | None -> xml := ours)
      | Conflict _ ->
        (* Unresolved conflict defaults to ours *)
        (match f.xml_loc with
         | Some loc -> xml := splice_field !xml ours loc
         | None -> xml := ours)
    ) fields;
  !xml

let merge_generic
    ~(base : Xml.t) ~(ours : Xml.t) ~(theirs : Xml.t)
    ~(resolutions : (string, Conflict.resolution) Hashtbl.t)
    ~(conflict_path : string) : Xml.t =
  let diffs = Xml_compare.compare_three_way ~base ~ours ~theirs in
  let xml = ref base in
  List.iter (fun (d : Xml_compare.field_diff) ->
      let field_path = conflict_path ^ "/" ^ d.Xml_compare.field_name in
      let resolution = Hashtbl.find_opt resolutions field_path in
      let source = match resolution with
        | Some Conflict.Ours -> ours
        | Some Conflict.Theirs -> theirs
        | Some Conflict.Base -> base
        | None -> ours
      in
      (match d.Xml_compare.kind with
       | Xml_compare.Attribute_diff ->
         (match d.Xml_compare.ours_value, d.Xml_compare.theirs_value,
                resolution with
         | Some ours_v, _, Some Conflict.Ours ->
           xml := replace_attr !xml d.Xml_compare.field_name ours_v
         | _, Some theirs_v, Some Conflict.Theirs ->
           xml := replace_attr !xml d.Xml_compare.field_name theirs_v
         | _, _, (Some Conflict.Base | None) ->
           (match d.Xml_compare.base_value with
            | Some base_v ->
              xml := replace_attr !xml d.Xml_compare.field_name base_v
            | None -> ())
         | _, _, _ -> ())
       | Xml_compare.Child_element_diff ->
         (match d.Xml_compare.kind with
          | Xml_compare.Child_element_diff ->
            (match find_child_by_tag source d.Xml_compare.field_name with
             | Some child ->
               xml := replace_child_by_tag !xml d.Xml_compare.field_name child
             | None -> xml := remove_child_by_tag !xml d.Xml_compare.field_name)
          | _ -> ())
       | Xml_compare.Text_content_diff ->
         xml := source)
    ) diffs;
  !xml
