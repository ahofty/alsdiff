(** Advanced streaming NFA tests — MultiWildcard, SingleWildcard, ParentNode,
    Regex, CurrentNode, and combined patterns against t4.xml. *)

open Alsdiff_base

let resolve_test_data_path filename =
  if Sys.file_exists ("data/" ^ filename) then "data/" ^ filename
  else if Sys.file_exists filename then filename
  else if Sys.file_exists ("test/data/" ^ filename) then "test/data/" ^ filename
  else if Sys.file_exists ("test/" ^ filename) then "test/" ^ filename
  else failwith (Printf.sprintf "Cannot find test data file: %s" filename)

let run_queries queries =
  let nfa = Upath2.compile queries in
  let path = resolve_test_data_path "t4.xml" in
  let stream = Xml2.stream_from_file path in
  Upath2.evaluate nfa stream

let q ?(attr = None) qid path_str =
  Upath2.query_of_path ~qid ~path_str ~attr

let find_by_qid results qid =
  List.filter (fun r -> r.Upath2.query_id = qid) results

let get_value r = Upath2.get_attr r "Value"

(* --- MultiWildcard tests --- *)

let test_multiwildcard_track () =
  let results = run_queries [ q 0 "/**/AudioTrack" ] in
  let matches = find_by_qid results 0 in
  let ids = List.filter_map (fun r -> Upath2.get_attr r "Id") matches in
  Alcotest.(check int) "one AudioTrack" 1 (List.length ids);
  Alcotest.(check string) "AudioTrack Id" "15" (List.hd ids)

let test_multiwildcard_manual () =
  let results = run_queries [ q 0 "/**/Manual" ~attr:(Some "Value") ] in
  let matches = find_by_qid results 0 in
  let values = List.filter_map get_value matches in
  Alcotest.(check bool) "multiple Manual matches" true (List.length values >= 3);
  (* Must contain Volume/Manual = "1" *)
  Alcotest.(check bool) "contains '1'" true (List.mem "1" values);
  (* Must contain On/Manual = "true" *)
  Alcotest.(check bool) "contains 'true'" true (List.mem "true" values)

let test_multiwildcard_effective_name () =
  let results = run_queries [ q 0 "/**/EffectiveName" ~attr:(Some "Value") ] in
  let matches = find_by_qid results 0 in
  let values = List.filter_map get_value matches in
  Alcotest.(check bool) "at least 3 EffectiveName matches" true (List.length values >= 3);
  Alcotest.(check bool) "contains 1-Tela" true (List.mem "1-Tela" values);
  Alcotest.(check bool) "contains 2-Audio" true (List.mem "2-Audio" values);
  Alcotest.(check bool) "contains Main" true (List.mem "Main" values)

(* --- SingleWildcard tests --- *)

let test_singlewildcard_name () =
  let results = run_queries [ q 0 "/Tracks/*/Name/EffectiveName" ~attr:(Some "Value") ] in
  let matches = find_by_qid results 0 in
  let values = List.filter_map get_value matches in
  Alcotest.(check int) "two tracks" 2 (List.length values);
  Alcotest.(check bool) "contains 1-Tela" true (List.mem "1-Tela" values);
  Alcotest.(check bool) "contains 2-Audio" true (List.mem "2-Audio" values)

let test_singlewildcard_mixer_volume () =
  let results = run_queries [ q 0 "/Tracks/*/DeviceChain/Mixer/Volume/Manual" ~attr:(Some "Value") ] in
  let matches = find_by_qid results 0 in
  let values = List.filter_map get_value matches in
  Alcotest.(check int) "two volume manuals" 2 (List.length values);
  List.iter (fun v ->
      Alcotest.(check string) "volume manual = 1" "1" v
    ) values

(* --- Regex tests --- *)

let test_regex_track_type () =
  let results = run_queries [ q 0 "/Tracks/'(Midi|Audio)Track'" ] in
  let matches = find_by_qid results 0 in
  let ids = List.filter_map (fun r -> Upath2.get_attr r "Id") matches in
  Alcotest.(check int) "two tracks" 2 (List.length ids);
  Alcotest.(check bool) "contains MidiTrack Id=14" true (List.mem "14" ids);
  Alcotest.(check bool) "contains AudioTrack Id=15" true (List.mem "15" ids)

(* --- ParentNode tests --- *)

let test_parentnode_on_manual () =
  let results = run_queries [ q 0 "/Mixer/On/LomId/../Manual" ~attr:(Some "Value") ] in
  let matches = find_by_qid results 0 in
  let values = List.filter_map get_value matches in
  (* Should find On/Manual across MidiTrack, AudioTrack, MainTrack *)
  Alcotest.(check bool) "at least 1 match" true (List.length values >= 1);
  Alcotest.(check bool) "contains 'true'" true (List.mem "true" values)

let test_parentnode_volume_manual () =
  let results = run_queries [ q 0 "/Mixer/Volume/LomId/../Manual" ~attr:(Some "Value") ] in
  let matches = find_by_qid results 0 in
  let values = List.filter_map get_value matches in
  Alcotest.(check bool) "at least 1 match" true (List.length values >= 1);
  Alcotest.(check bool) "contains '1'" true (List.mem "1" values)

let test_multiwildcard_parentnode () =
  let results = run_queries [ q 0 "/**/On/LomId/../Manual" ~attr:(Some "Value") ] in
  let matches = find_by_qid results 0 in
  let values = List.filter_map get_value matches in
  (* MultiWildcard + ParentNode: find LomId under On anywhere, navigate up to Manual sibling *)
  Alcotest.(check bool) "at least 1 match" true (List.length values >= 1);
  Alcotest.(check bool) "all values are 'true'"
    true (List.for_all (fun v -> v = "true") values)

(* --- CurrentNode tests --- *)

let test_currentnode_name () =
  let results = run_queries [ q 0 "/Tracks/MidiTrack/Name/." ] in
  let matches = find_by_qid results 0 in
  Alcotest.(check int) "one match" 1 (List.length matches);
  let r = List.hd matches in
  Alcotest.(check string) "element is Name" "Name" r.Upath2.element_name

let test_currentnode_effective_name () =
  let results = run_queries [ q 0 "/Tracks/MidiTrack/Name/EffectiveName/." ~attr:(Some "Value") ] in
  let matches = find_by_qid results 0 in
  Alcotest.(check int) "one match" 1 (List.length matches);
  Alcotest.(check string) "value is 1-Tela" "1-Tela" (Option.get (get_value (List.hd matches)))

(* --- Combined tests --- *)

let test_combined_volume_manual () =
  let results = run_queries [ q 0 "/**/Volume/Manual" ~attr:(Some "Value") ] in
  let matches = find_by_qid results 0 in
  let values = List.filter_map get_value matches in
  Alcotest.(check bool) "at least 2 matches" true (List.length values >= 2);
  Alcotest.(check bool) "contains '1'" true (List.mem "1" values)

let test_combined_multiwildcard_regex () =
  let results = run_queries [ q 0 "/**/'(Midi|Audio)Track'/Name/EffectiveName" ~attr:(Some "Value") ] in
  let matches = find_by_qid results 0 in
  let values = List.filter_map get_value matches in
  Alcotest.(check int) "two tracks" 2 (List.length values);
  Alcotest.(check bool) "contains 1-Tela" true (List.mem "1-Tela" values);
  Alcotest.(check bool) "contains 2-Audio" true (List.mem "2-Audio" values)

let () =
  Alcotest.run "Streaming Advanced" [
    "multiwildcard", [
      Alcotest.test_case "AudioTrack at any depth" `Quick test_multiwildcard_track;
      Alcotest.test_case "Manual at any depth" `Quick test_multiwildcard_manual;
      Alcotest.test_case "EffectiveName at any depth" `Quick test_multiwildcard_effective_name;
    ];
    "singlewildcard", [
      Alcotest.test_case "*/Name/EffectiveName" `Quick test_singlewildcard_name;
      Alcotest.test_case "*/DeviceChain/Mixer/Volume/Manual" `Quick test_singlewildcard_mixer_volume;
    ];
    "regex", [
      Alcotest.test_case "(Midi|Audio)Track regex" `Quick test_regex_track_type;
    ];
    "parentnode", [
      Alcotest.test_case "On/LomId/../Manual" `Quick test_parentnode_on_manual;
      Alcotest.test_case "Volume/LomId/../Manual" `Quick test_parentnode_volume_manual;
      Alcotest.test_case "/**/On/LomId/../Manual" `Quick test_multiwildcard_parentnode;
    ];
    "currentnode", [
      Alcotest.test_case "Name/." `Quick test_currentnode_name;
      Alcotest.test_case "EffectiveName/." `Quick test_currentnode_effective_name;
    ];
    "combined", [
      Alcotest.test_case "/**/Volume/Manual" `Quick test_combined_volume_manual;
      Alcotest.test_case "/**/(Midi|Audio)Track/Name/EffectiveName" `Quick test_combined_multiwildcard_regex;
    ];
  ]
