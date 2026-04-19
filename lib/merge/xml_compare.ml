open Alsdiff_base

type field_kind =
  | Attribute_diff
  | Child_element_diff
  | Text_content_diff

type field_diff = {
  field_name : string;
  kind : field_kind;
  ours_value : string option;
  theirs_value : string option;
  base_value : string option;
}

let compare_attrs ~base_attrs ~ours_attrs ~theirs_attrs =
  let all_keys = Hashtbl.create 8 in
  List.iter (fun (k, _) -> Hashtbl.replace all_keys k ()) base_attrs;
  List.iter (fun (k, _) -> Hashtbl.replace all_keys k ()) ours_attrs;
  List.iter (fun (k, _) -> Hashtbl.replace all_keys k ()) theirs_attrs;
  let diffs = ref [] in
  Hashtbl.iter (fun key () ->
      let base_v = List.assoc_opt key base_attrs in
      let ours_v = List.assoc_opt key ours_attrs in
      let theirs_v = List.assoc_opt key theirs_attrs in
      if ours_v <> theirs_v || ours_v <> base_v then
        diffs := { field_name = key; kind = Attribute_diff;
                   ours_value = ours_v; theirs_value = theirs_v; base_value = base_v }
                 :: !diffs
    ) all_keys;
  List.rev !diffs

let group_children_by_tag (childs : Xml.t list) : (string * Xml.t list) list =
  let tbl = Hashtbl.create 8 in
  let order = ref [] in
  List.iter (fun child ->
      match child with
      | Xml.Element { name; _ } ->
        if not (Hashtbl.mem tbl name) then (
          Hashtbl.replace tbl name [];
          order := name :: !order
        );
        Hashtbl.replace tbl name (child :: Hashtbl.find tbl name)
      | Xml.Data _ -> ()
    ) childs;
  let result = List.filter_map (fun name ->
      match Hashtbl.find_opt tbl name with
      | Some elems -> Some (name, List.rev elems)
      | None -> None
    ) (List.rev !order)
  in
  result

let compare_text_content ~base ~ours ~theirs =
  let base_text = match base with
    | [Xml.Data s] -> Some s | _ -> None
  in
  let ours_text = match ours with
    | [Xml.Data s] -> Some s | _ -> None
  in
  let theirs_text = match theirs with
    | [Xml.Data s] -> Some s | _ -> None
  in
  if ours_text <> theirs_text || ours_text <> base_text then
    [{ field_name = "text"; kind = Text_content_diff;
       ours_value = ours_text; theirs_value = theirs_text; base_value = base_text }]
  else []

let xml_to_short_string xml =
  match xml with
  | Xml.Element { name; attrs; childs } ->
    let attrs_str = List.map (fun (k, v) ->
        Fmt.str "%s=\"%s\"" k (Xml.escape_xml v)
      ) attrs |> String.concat " " in
    let inner = match childs with
      | [] -> ""
      | [Xml.Data s] -> Xml.escape_xml s
      | cs -> Fmt.str "[%d children]" (List.length cs)
    in
    let spaced = if attrs_str = "" then "" else " " in
    Fmt.str "<%s%s%s>%s</%s>" name spaced attrs_str inner name
  | Xml.Data s -> Xml.escape_xml s

let compare_child_groups ~base_groups ~ours_groups ~theirs_groups =
  let diffs = ref [] in
  let all_tags = Hashtbl.create 8 in
  List.iter (fun (tag, _) -> Hashtbl.replace all_tags tag ()) base_groups;
  List.iter (fun (tag, _) -> Hashtbl.replace all_tags tag ()) ours_groups;
  List.iter (fun (tag, _) -> Hashtbl.replace all_tags tag ()) theirs_groups;
  Hashtbl.iter (fun tag () ->
      let base_elems = List.assoc_opt tag base_groups
        |> Option.value ~default:[] in
      let ours_elems = List.assoc_opt tag ours_groups
        |> Option.value ~default:[] in
      let theirs_elems = List.assoc_opt tag theirs_groups
        |> Option.value ~default:[] in
      let base_count = List.length base_elems in
      let ours_count = List.length ours_elems in
      let theirs_count = List.length theirs_elems in
      if ours_count <> theirs_count || ours_count <> base_count then
        let ours_str = match ours_elems with
          | [] -> None
          | [e] -> Some (xml_to_short_string e)
          | es -> Some (Fmt.str "%d x <%s>" (List.length es) tag)
        in
        let theirs_str = match theirs_elems with
          | [] -> None
          | [e] -> Some (xml_to_short_string e)
          | es -> Some (Fmt.str "%d x <%s>" (List.length es) tag)
        in
        let base_str = match base_elems with
          | [] -> None
          | [e] -> Some (xml_to_short_string e)
          | es -> Some (Fmt.str "%d x <%s>" (List.length es) tag)
        in
        diffs := { field_name = tag; kind = Child_element_diff;
                   ours_value = ours_str; theirs_value = theirs_str;
                   base_value = base_str }
                 :: !diffs
      else
        (* Same count — compare first elements structurally *)
        let differs = List.exists2 (fun o t -> not (Xml.equal o t))
            ours_elems theirs_elems
        in
        if differs then begin
          let ours_str = match ours_elems with
            | [] -> None
            | [e] -> Some (xml_to_short_string e)
            | es -> Some (Fmt.str "%d x <%s>" (List.length es) tag)
          in
          let theirs_str = match theirs_elems with
            | [] -> None
            | [e] -> Some (xml_to_short_string e)
            | es -> Some (Fmt.str "%d x <%s>" (List.length es) tag)
          in
          let base_str = match base_elems with
            | [] -> None
            | [e] -> Some (xml_to_short_string e)
            | es -> Some (Fmt.str "%d x <%s>" (List.length es) tag)
          in
          diffs := { field_name = tag; kind = Child_element_diff;
                     ours_value = ours_str; theirs_value = theirs_str;
                     base_value = base_str }
                   :: !diffs
        end
    ) all_tags;
  List.rev !diffs

let compare_three_way ~base ~ours ~theirs =
  match base, ours, theirs with
  | Xml.Element { name = base_name; attrs = base_attrs; childs = base_childs },
    Xml.Element { name = ours_name; attrs = ours_attrs; childs = ours_childs },
    Xml.Element { name = theirs_name; attrs = theirs_attrs; childs = theirs_childs } ->
    let name_diff =
      if not (String.equal base_name ours_name && String.equal base_name theirs_name) then
        [{ field_name = "tag_name"; kind = Attribute_diff;
           ours_value = Some ours_name; theirs_value = Some theirs_name;
           base_value = Some base_name }]
      else []
    in
    let attr_diffs = compare_attrs ~base_attrs ~ours_attrs ~theirs_attrs in
    let base_groups = group_children_by_tag base_childs in
    let ours_groups = group_children_by_tag ours_childs in
    let theirs_groups = group_children_by_tag theirs_childs in
    let child_diffs = compare_child_groups ~base_groups ~ours_groups ~theirs_groups in
    name_diff @ attr_diffs @ child_diffs
  | _ -> []
