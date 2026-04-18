open Alsdiff_base
open Alsdiff_base.Diff
open Alsdiff_live

type merge_action =
  | Keep
  | Take_ours
  | Take_theirs
  | Both_agree
  | Recurse of merge_field list
  | Conflict of Conflict.t

and merge_field = {
  field_name : string;
  action : merge_action;
}

type entity_merge =
  | Entity_keep
  | Entity_add of Xml.t
  | Entity_remove of Xml.t
  | Entity_modify of Xml.t * Xml.t
  | Entity_modify_both of merge_action * Xml.t * Xml.t * Xml.t
  | Entity_conflict of Conflict.t

let pp_merge_action fmt = function
  | Keep -> Fmt.string fmt "Keep"
  | Take_ours -> Fmt.string fmt "Take_ours"
  | Take_theirs -> Fmt.string fmt "Take_theirs"
  | Both_agree -> Fmt.string fmt "Both_agree"
  | Recurse _ -> Fmt.string fmt "Recurse(...)"
  | Conflict c -> Fmt.pf fmt "Conflict(%a)" Conflict.pp c

let collect_conflicts action =
  let rec aux acc = function
    | Keep | Take_ours | Take_theirs | Both_agree -> acc
    | Conflict c -> c :: acc
    | Recurse fields -> List.fold_left (fun acc f -> aux acc f.action) acc fields
  in
  aux [] action

(* Atomic update comparison *)
let compare_atomic_update path (ours : 'a atomic_update) (theirs : 'a atomic_update)
    ~equal ~pp_value =
  match ours, theirs with
  | `Unchanged, `Unchanged -> Keep
  | `Modified _, `Unchanged -> Take_ours
  | `Unchanged, `Modified _ -> Take_theirs
  | `Modified ours_patch, `Modified theirs_patch ->
    if equal ours_patch.newval theirs_patch.newval
    then Both_agree
    else
      Conflict {
        Conflict.path;
        kind = Atomic_conflict;
        ours_desc = Fmt.str "%a" pp_value ours_patch.newval;
        theirs_desc = Fmt.str "%a" pp_value theirs_patch.newval;
      }

(* Structured update comparison *)
let compare_structured_update _path
    (ours : 'p structured_update) (theirs : 'p structured_update)
    ~recurse ~is_empty =
  match ours, theirs with
  | `Unchanged, `Unchanged -> Keep
  | `Modified _, `Unchanged -> Take_ours
  | `Unchanged, `Modified _ -> Take_theirs
  | `Modified ours_patch, `Modified theirs_patch ->
    if is_empty ours_patch && is_empty theirs_patch then Keep
    else if is_empty ours_patch then Take_theirs
    else if is_empty theirs_patch then Take_ours
    else Recurse (recurse ours_patch theirs_patch)

(* Compare entity changes with base context for 3-way diff.
   compare_patches receives (base_entity, ours_entity, theirs_entity). *)
let compare_entity_changes path
    (ours_changes : ('a, 'p) structured_change list)
    (theirs_changes : ('a, 'p) structured_change list)
    ~base_entities ~ours_entities ~theirs_entities
    ~get_xml ~get_name ~has_same_id:_
    ~compare_patches =
  let base_lookup = Hashtbl.create 16 in
  let ours_lookup = Hashtbl.create 16 in
  let theirs_lookup = Hashtbl.create 16 in

  let build_lookup tbl entities changes =
    List.iter (fun e ->
        Hashtbl.replace tbl (get_name e) e
      ) entities;
    List.iter (fun c ->
        match c with
        | `Added a ->
          Hashtbl.replace tbl (get_name a) a
        | `Removed a ->
          Hashtbl.replace tbl (get_name a) a
        | `Modified _ | `Unchanged -> ()
      ) changes
  in
  List.iter (fun e ->
      Hashtbl.replace base_lookup (get_name e) e
    ) base_entities;
  build_lookup ours_lookup ours_entities ours_changes;
  build_lookup theirs_lookup theirs_entities theirs_changes;

  let results = ref [] in
  let conflicts = ref [] in

  let all_names = Hashtbl.create 32 in
  List.iter (fun e -> Hashtbl.replace all_names (get_name e) ()) ours_entities;
  List.iter (fun e -> Hashtbl.replace all_names (get_name e) ()) theirs_entities;
  List.iter (fun c ->
      match c with
      | `Added a | `Removed a ->
        Hashtbl.replace all_names (get_name a) ()
      | `Modified _ | `Unchanged -> ()
    ) ours_changes;
  List.iter (fun c ->
      match c with
      | `Added a | `Removed a ->
        Hashtbl.replace all_names (get_name a) ()
      | `Modified _ | `Unchanged -> ()
    ) theirs_changes;

  let classify changes =
    let added = Hashtbl.create 8 in
    let removed = Hashtbl.create 8 in
    List.iter (fun c ->
        match c with
        | `Added a -> Hashtbl.replace added (get_name a) a
        | `Removed a -> Hashtbl.replace removed (get_name a) a
        | `Modified _ | `Unchanged -> ()
      ) changes;
    added, removed
  in

  let ours_added, ours_removed = classify ours_changes in
  let theirs_added, theirs_removed = classify theirs_changes in

  Hashtbl.iter (fun name () ->
      let base_entity = Hashtbl.find_opt base_lookup name in
      let ours_entity = Hashtbl.find_opt ours_lookup name in
      let theirs_entity = Hashtbl.find_opt theirs_lookup name in

      let ours_was_added = Hashtbl.find_opt ours_added name in
      let ours_was_removed = Hashtbl.find_opt ours_removed name in
      let theirs_was_added = Hashtbl.find_opt theirs_added name in
      let theirs_was_removed = Hashtbl.find_opt theirs_removed name in

      let base_xml () = match base_entity with
        | Some be -> get_xml be
        | None -> (* defensive fallback *) Xml.Element { name = "none"; attrs = []; childs = [] }
      in

      match ours_was_added, ours_was_removed, theirs_was_added, theirs_was_removed with
      | Some a, None, None, None
      | Some a, None, Some _, None ->
        results := Entity_add (get_xml a) :: !results
      | None, None, Some a, None
      | None, None, Some a, Some _ ->
        results := Entity_add (get_xml a) :: !results
      | None, Some _, None, None
      | None, Some _, None, Some _ ->
        results := Entity_remove (base_xml ()) :: !results
      | None, None, None, Some _ ->
        results := Entity_remove (base_xml ()) :: !results
      | Some _, None, None, Some _ ->
        let c = {
          Conflict.path = path ^ "/" ^ name;
          kind = Add_remove_conflict;
          ours_desc = "added";
          theirs_desc = "removed";
        } in
        conflicts := c :: !conflicts;
        results := Entity_conflict c :: !results
      | None, Some _, Some _, None ->
        let c = {
          Conflict.path = path ^ "/" ^ name;
          kind = Add_remove_conflict;
          ours_desc = "removed";
          theirs_desc = "added";
        } in
        conflicts := c :: !conflicts;
        results := Entity_conflict c :: !results
      | Some _, Some _, Some _, Some _ ->
        results := Entity_keep :: !results
      | None, None, None, None ->
        (* Neither added nor removed — check if both modified *)
        (match ours_entity, theirs_entity with
         | Some oe, Some te ->
           let action = match base_entity with
             | Some be -> compare_patches be oe te
             | None -> Keep
           in
           let new_conflicts = collect_conflicts action in
           conflicts := new_conflicts @ !conflicts;
           results := Entity_modify_both (action, base_xml (), get_xml oe, get_xml te) :: !results
         | Some oe, None ->
           results := Entity_modify (base_xml (), get_xml oe) :: !results
         | None, Some te ->
           results := Entity_modify (base_xml (), get_xml te) :: !results
         | None, None ->
           results := Entity_keep :: !results)
      | _ ->
        results := Entity_keep :: !results
    ) all_names;

  List.rev !results, List.rev !conflicts

(* Track patch comparison *)
let compare_track_patch path ours_patch theirs_patch =
  let fields = ref [] in
  let conflicts = ref [] in

  let add_field name action =
    fields := { field_name = name; action } :: !fields;
    conflicts := collect_conflicts action @ !conflicts
  in

  (match ours_patch, theirs_patch with
   | Track.Patch.MidiPatch op, Track.Patch.MidiPatch tp ->
     add_field "name" (compare_atomic_update
                         (path ^ "/name") op.Track.MidiTrack.Patch.name tp.Track.MidiTrack.Patch.name
                         ~equal:String.equal
                         ~pp_value:(fun fmt s -> Fmt.string fmt s));
     add_field "devices" Keep;
     add_field "mixer" Keep
   | Track.Patch.AudioPatch op, Track.Patch.AudioPatch tp ->
     add_field "name" (compare_atomic_update
                         (path ^ "/name") op.Track.AudioTrack.Patch.name tp.Track.AudioTrack.Patch.name
                         ~equal:String.equal
                         ~pp_value:(fun fmt s -> Fmt.string fmt s));
     add_field "devices" Keep;
     add_field "mixer" Keep
   | Track.Patch.MainPatch _, Track.Patch.MainPatch _ ->
     add_field "mixer" Keep
   | _ ->
     add_field "track" Keep);

  Recurse (List.rev !fields), List.rev !conflicts

(* Device patch comparison *)
let compare_device_patch path ours_patch theirs_dev =
  let get_name = function
    | Device.Regular d -> d.device_name
    | Device.Plugin d -> d.device_name
    | Device.Max4Live d -> d.device_name
    | Device.Group d -> d.device_name
  in
  let dev_name = get_name theirs_dev in
  (match ours_patch with
   | Device.RegularPatch op ->
     compare_atomic_update (path ^ "/" ^ dev_name) op.display_name op.display_name
       ~equal:String.equal ~pp_value:(fun fmt s -> Fmt.string fmt s)
   | Device.PluginPatch op ->
     compare_atomic_update (path ^ "/" ^ dev_name) op.display_name op.display_name
       ~equal:String.equal ~pp_value:(fun fmt s -> Fmt.string fmt s)
   | Device.Max4LivePatch op ->
     compare_atomic_update (path ^ "/" ^ dev_name) op.display_name op.display_name
       ~equal:String.equal ~pp_value:(fun fmt s -> Fmt.string fmt s)
   | Device.GroupPatch op ->
     compare_atomic_update (path ^ "/" ^ dev_name) op.display_name op.display_name
       ~equal:String.equal ~pp_value:(fun fmt s -> Fmt.string fmt s))

(* Top-level liveset patch comparison *)
let three_way_compare
    ~base:(base_ls : Liveset.t)
    ~ours:(ours_ls : Liveset.t)
    ~theirs:(theirs_ls : Liveset.t)
    ~ours_patch:(ours_patch : Liveset.Patch.t)
    ~theirs_patch:(theirs_patch : Liveset.Patch.t) =
  let fields = ref [] in
  let all_conflicts = ref [] in

  let add_field name action =
    fields := { field_name = name; action } :: !fields;
    all_conflicts := collect_conflicts action @ !all_conflicts
  in

  add_field "name" (compare_atomic_update
                      "name" ours_patch.name theirs_patch.name
                      ~equal:String.equal
                      ~pp_value:(fun fmt s -> Fmt.string fmt s));
  add_field "creator" (compare_atomic_update
                         "creator" ours_patch.creator theirs_patch.creator
                         ~equal:String.equal
                         ~pp_value:(fun fmt s -> Fmt.string fmt s));

  let main_action =
    compare_structured_update "MainTrack" ours_patch.main theirs_patch.main
      ~is_empty:(fun (p : Track.MainTrack.Patch.t) ->
          Track.MainTrack.Patch.is_empty p)
      ~recurse:(fun _ours_p _theirs_p ->
          [{ field_name = "main_track"; action = Keep }])
  in
  add_field "main" main_action;

  let track_get_xml t =
    match t with
    | Track.Midi m -> m.xml
    | Track.Audio a | Track.Group a | Track.Return a -> a.xml
    | Track.Main m -> m.xml
  in

  let track_results, track_conflicts =
    compare_entity_changes "Tracks"
      ours_patch.tracks theirs_patch.tracks
      ~base_entities:base_ls.tracks
      ~ours_entities:ours_ls.tracks ~theirs_entities:theirs_ls.tracks
      ~get_xml:track_get_xml
      ~get_name:Track.get_name
      ~has_same_id:Track.has_same_id
      ~compare_patches:(fun base_a ours_a theirs_a ->
          let ours_p = Track.diff base_a ours_a in
          let theirs_p = Track.diff base_a theirs_a in
          compare_track_patch
            ("Tracks/" ^ Track.get_name ours_a)
            ours_p theirs_p
          |> fst)
  in
  all_conflicts := track_conflicts @ !all_conflicts;
  fields := { field_name = "tracks"; action = Keep } :: !fields;

  let return_results, return_conflicts =
    compare_entity_changes "Returns"
      ours_patch.returns theirs_patch.returns
      ~base_entities:base_ls.returns
      ~ours_entities:ours_ls.returns ~theirs_entities:theirs_ls.returns
      ~get_xml:track_get_xml
      ~get_name:Track.get_name
      ~has_same_id:Track.has_same_id
      ~compare_patches:(fun base_a ours_a theirs_a ->
          let ours_p = Track.diff base_a ours_a in
          let theirs_p = Track.diff base_a theirs_a in
          compare_track_patch
            ("Returns/" ^ Track.get_name ours_a)
            ours_p theirs_p
          |> fst)
  in
  all_conflicts := return_conflicts @ !all_conflicts;
  fields := { field_name = "returns"; action = Keep } :: !fields;

  let locator_results, locator_conflicts =
    compare_entity_changes "Locators"
      ours_patch.locators theirs_patch.locators
      ~base_entities:base_ls.locators
      ~ours_entities:ours_ls.locators ~theirs_entities:theirs_ls.locators
      ~get_xml:(fun l -> l.xml)
      ~get_name:(fun l -> Printf.sprintf "Locator(%d)" l.Liveset.Locator.id)
      ~has_same_id:Liveset.Locator.has_same_id
      ~compare_patches:(fun _base_a _ours_a _theirs_a -> Keep)
  in
  all_conflicts := locator_conflicts @ !all_conflicts;
  fields := { field_name = "locators"; action = Keep } :: !fields;

  let action = Recurse (List.rev !fields) in
  action, List.rev !all_conflicts,
  track_results, return_results, locator_results
