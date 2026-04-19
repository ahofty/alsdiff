open Alsdiff_base
open Alsdiff_base.Diff
open Alsdiff_live
open Patch_compare

let update_root_attr xml attr_name value =
  match xml with
  | Xml.Element { name; attrs; childs } ->
    let attrs = List.map (fun (k, v) ->
        if String.equal k attr_name then (k, value) else (k, v)
      ) attrs in
    Xml.Element { name; attrs; childs }
  | Xml.Data _ -> xml

let resolve_action_tree = Xml_merge_fields.resolve_action_tree

let apply_merge
    ~base_xml ~base:_ ~ours:_ ~theirs:_
    ~(action : merge_action)
    ~(track_merges : entity_merge list)
    ~(return_merges : entity_merge list)
    ~(locator_merges : entity_merge list)
    ~(ours_patch : Liveset.Patch.t)
    ~(theirs_patch : Liveset.Patch.t)
    ?(resolutions : (string, Conflict.resolution) Hashtbl.t option)
    () =
  let resolutions = match resolutions with Some r -> r | None -> Hashtbl.create 0 in
  let xml = ref base_xml in
  ignore action;

  let apply_entity_list merges =
    List.iter (function
        | Entity_keep -> ()
        | Entity_add new_xml ->
          xml := Xml.add_child !xml new_xml
        | Entity_remove base_entity_xml ->
          xml := Xml.remove_child !xml ~child:base_entity_xml
        | Entity_modify (base_entity_xml, new_xml) ->
          xml := Xml.replace_child !xml ~old:base_entity_xml ~replacement:new_xml
        | Entity_modify_both (action, base_entity_xml, ours_xml, theirs_xml) ->
          let resolved = resolve_action_tree resolutions action in
          (match resolved with
           | Take_ours | Both_agree ->
             xml := Xml.replace_child !xml ~old:base_entity_xml ~replacement:ours_xml
           | Take_theirs ->
             xml := Xml.replace_child !xml ~old:base_entity_xml ~replacement:theirs_xml
           | Keep -> ()
           | Recurse fields ->
             let merged = Xml_merge_fields.merge_from_fields
                 ~base:base_entity_xml ~ours:ours_xml ~theirs:theirs_xml
                 ~fields ~resolutions
             in
             xml := Xml.replace_child !xml ~old:base_entity_xml ~replacement:merged
           | Conflict c ->
             let has_field_resolutions =
               let prefix = c.Conflict.path ^ "/" in
               let found = ref false in
               Hashtbl.iter (fun k _ ->
                   if String.starts_with ~prefix k then found := true
                 ) resolutions;
               !found
             in
             if has_field_resolutions then
               let merged = Xml_merge_fields.merge_generic
                   ~base:base_entity_xml ~ours:ours_xml ~theirs:theirs_xml
                   ~resolutions ~conflict_path:c.Conflict.path
               in
               xml := Xml.replace_child !xml ~old:base_entity_xml ~replacement:merged
             else
               xml := Xml.replace_child !xml ~old:base_entity_xml ~replacement:ours_xml)
        | Entity_conflict (c, ours_xml, theirs_xml) ->
          (match Hashtbl.find_opt resolutions c.Conflict.path with
           | Some Conflict.Ours ->
             xml := Xml.replace_child !xml ~old:base_xml ~replacement:ours_xml
           | Some Conflict.Theirs ->
             xml := Xml.replace_child !xml ~old:base_xml ~replacement:theirs_xml
           | Some Conflict.Base -> ()
           | None -> ()))
      merges
  in

  let apply_atomic attr_name
      (ours_upd : string atomic_update) (theirs_upd : string atomic_update) =
    match ours_upd, theirs_upd with
    | `Unchanged, `Unchanged -> ()
    | `Modified p, `Unchanged ->
      xml := update_root_attr !xml attr_name p.newval
    | `Unchanged, `Modified p ->
      xml := update_root_attr !xml attr_name p.newval
    | `Modified p, `Modified _ ->
      xml := update_root_attr !xml attr_name p.newval
  in

  apply_atomic "Creator" ours_patch.creator theirs_patch.creator;

  (* Apply entity-level merges *)
  apply_entity_list track_merges;
  apply_entity_list return_merges;
  apply_entity_list locator_merges;

  !xml
