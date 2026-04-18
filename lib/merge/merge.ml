open Alsdiff_base
open Alsdiff_live

type merge_result = {
  xml : Xml.t;
  conflicts : Conflict.t list;
  clean : bool;
}

let three_way_merge ~base_file ~ours_file ~theirs_file =
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

  let merged_xml =
    Xml_merge.apply_merge
      ~base_xml ~base:base_ls ~ours:ours_ls ~theirs:theirs_ls
      ~action ~track_merges ~return_merges ~locator_merges
      ~ours_patch ~theirs_patch
  in

  let clean = conflicts = [] in
  { xml = merged_xml; conflicts; clean }

let merge_to_file ~output_file ~base_file ~ours_file ~theirs_file =
  let result = three_way_merge ~base_file ~ours_file ~theirs_file in
  File.write_als output_file result.xml;
  List.iter (fun c ->
      Fmt.epr "%a@." Conflict.pp c
    ) result.conflicts;
  if result.clean then 0 else 1
