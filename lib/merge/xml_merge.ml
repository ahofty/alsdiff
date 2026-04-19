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

let rec resolve_action_tree
    (resolutions : (string, Conflict.resolution) Hashtbl.t)
    (action : merge_action) : merge_action =
  match action with
  | Recurse fields ->
    Recurse (List.map (fun f ->
        { f with action = resolve_action_tree resolutions f.action }
      ) fields)
  | Conflict c ->
    (match Hashtbl.find_opt resolutions c.Conflict.path with
     | Some Conflict.Ours -> Take_ours
     | Some Conflict.Theirs -> Take_theirs
     | Some Conflict.Base -> Keep
     | None -> Take_ours)
  | other -> other

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
           | Take_ours | Both_agree | Recurse _ ->
             xml := Xml.replace_child !xml ~old:base_entity_xml ~replacement:ours_xml
           | Take_theirs ->
             xml := Xml.replace_child !xml ~old:base_entity_xml ~replacement:theirs_xml
           | Keep -> ()
           | Conflict _ ->
             xml := Xml.replace_child !xml ~old:base_entity_xml ~replacement:ours_xml)
        | Entity_conflict c ->
          (match Hashtbl.find_opt resolutions c.Conflict.path with
           | Some Conflict.Ours ->
             xml := Xml.replace_child !xml ~old:base_xml ~replacement:base_xml
           | Some Conflict.Theirs ->
             xml := Xml.replace_child !xml ~old:base_xml ~replacement:base_xml
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
