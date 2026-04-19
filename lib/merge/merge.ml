open Alsdiff_base
open Alsdiff_live

type merge_result = {
  xml : Xml.t;
  conflicts : Conflict.t list;
  clean : bool;
}

type merge_context = {
  base_xml : Xml.t;
  ours_xml : Xml.t;
  theirs_xml : Xml.t;
  base_ls : Liveset.t;
  ours_ls : Liveset.t;
  theirs_ls : Liveset.t;
  ours_patch : Liveset.Patch.t;
  theirs_patch : Liveset.Patch.t;
  action : Patch_compare.merge_action;
  conflicts : Conflict.t list;
  track_merges : Patch_compare.entity_merge list;
  return_merges : Patch_compare.entity_merge list;
  locator_merges : Patch_compare.entity_merge list;
  clean : bool;
}

let three_way_merge_context ~base_file ~ours_file ~theirs_file =
  let base_xml = File.open_als base_file in
  let ours_xml = File.open_als ours_file in
  let theirs_xml = File.open_als theirs_file in

  let base_ls = Liveset.create base_xml base_file in
  let ours_ls = Liveset.create ours_xml ours_file in
  let theirs_ls = Liveset.create theirs_xml theirs_file in

  let ours_patch = Liveset.diff base_ls ours_ls in
  let theirs_patch = Liveset.diff base_ls theirs_ls in

  let action, conflicts, track_merges, return_merges, locator_merges =
    Patch_compare.three_way_compare
      ~base:base_ls ~ours:ours_ls ~theirs:theirs_ls
      ~ours_patch ~theirs_patch
  in

  let clean = conflicts = [] in
  { base_xml; ours_xml; theirs_xml;
    base_ls; ours_ls; theirs_ls;
    ours_patch; theirs_patch;
    action; conflicts;
    track_merges; return_merges; locator_merges;
    clean }

let apply_context_resolutions
    (ctx : merge_context)
    (resolutions : (string, Conflict.resolution) Hashtbl.t) : Xml.t =
  Xml_merge.apply_merge
    ~base_xml:ctx.base_xml ~base:ctx.base_ls ~ours:ctx.ours_ls ~theirs:ctx.theirs_ls
    ~action:ctx.action ~track_merges:ctx.track_merges
    ~return_merges:ctx.return_merges ~locator_merges:ctx.locator_merges
    ~ours_patch:ctx.ours_patch ~theirs_patch:ctx.theirs_patch
    ~resolutions ()

let three_way_merge ~base_file ~ours_file ~theirs_file =
  let ctx = three_way_merge_context ~base_file ~ours_file ~theirs_file in
  let merged_xml =
    Xml_merge.apply_merge
      ~base_xml:ctx.base_xml ~base:ctx.base_ls ~ours:ctx.ours_ls ~theirs:ctx.theirs_ls
      ~action:ctx.action ~track_merges:ctx.track_merges
      ~return_merges:ctx.return_merges ~locator_merges:ctx.locator_merges
      ~ours_patch:ctx.ours_patch ~theirs_patch:ctx.theirs_patch ()
  in
  { xml = merged_xml; conflicts = ctx.conflicts; clean = ctx.clean }

let merge_to_file ~output_file ~base_file ~ours_file ~theirs_file =
  let result = three_way_merge ~base_file ~ours_file ~theirs_file in
  File.write_als output_file result.xml;
  List.iter (fun c ->
      Fmt.epr "%a@." Conflict.pp c
    ) result.conflicts;
  if result.clean then 0 else 1
