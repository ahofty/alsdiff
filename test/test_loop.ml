open Alcotest
open Alsdiff_base
open Alsdiff_live.Clip.Loop
open Utils

let dummy_xml = Xml.read_string "<dummy/>"

let test_create_basic () =
  let xml = Xml.read_file (resolve_test_data_path "loop.xml") in
  let loop = create xml in
  check (float 0.001) "start_time" 92.0 loop.start_time;
  check (float 0.001) "end_time" 112.0 loop.end_time;
  check bool "on" false loop.on

let test_create_loop_enabled () =
  let xml = Xml.Element { name = "Loop"; attrs = []; childs = [
      Xml.Element { name = "LoopStart"; attrs = ["Value", "10.0"]; childs = [Xml.Data ""] };
      Xml.Element { name = "LoopEnd"; attrs = ["Value", "20.0"]; childs = [Xml.Data ""] };
      Xml.Element { name = "StartRelative"; attrs = ["Value", "0"]; childs = [Xml.Data ""] };
      Xml.Element { name = "LoopOn"; attrs = ["Value", "true"]; childs = [Xml.Data ""] };
      Xml.Element { name = "OutMarker"; attrs = ["Value", "20.0"]; childs = [Xml.Data ""] };
      Xml.Element { name = "HiddenLoopStart"; attrs = ["Value", "10.0"]; childs = [Xml.Data ""] };
      Xml.Element { name = "HiddenLoopEnd"; attrs = ["Value", "20.0"]; childs = [Xml.Data ""] };
    ]} in
  let loop = create xml in
  check (float 0.001) "start_time" 10.0 loop.start_time;
  check (float 0.001) "end_time" 20.0 loop.end_time;
  check bool "on" true loop.on

let test_create_different_times () =
  let xml = Xml.Element { name = "Loop"; attrs = []; childs = [
      Xml.Element { name = "LoopStart"; attrs = ["Value", "4.5"]; childs = [Xml.Data ""] };
      Xml.Element { name = "LoopEnd"; attrs = ["Value", "8.25"]; childs = [Xml.Data ""] };
      Xml.Element { name = "StartRelative"; attrs = ["Value", "0"]; childs = [Xml.Data ""] };
      Xml.Element { name = "LoopOn"; attrs = ["Value", "false"]; childs = [Xml.Data ""] };
      Xml.Element { name = "OutMarker"; attrs = ["Value", "8.25"]; childs = [Xml.Data ""] };
      Xml.Element { name = "HiddenLoopStart"; attrs = ["Value", "4.5"]; childs = [Xml.Data ""] };
      Xml.Element { name = "HiddenLoopEnd"; attrs = ["Value", "8.25"]; childs = [Xml.Data ""] };
    ]} in
  let loop = create xml in
  check (float 0.001) "start_time" 4.5 loop.start_time;
  check (float 0.001) "end_time" 8.25 loop.end_time;
  check bool "on" false loop.on

let test_invalid_element_name () =
  let xml = Xml.Element { name = "InvalidLoop"; attrs = []; childs = [] } in
  try
    let _ = create xml in
    fail "Expected Xml_error for invalid element name"
  with
  | Xml.Xml_error (_, msg) ->
    check string "error message" "Invalid XML element for creating Loop" msg

let test_missing_loop_start () =
  let xml = Xml.Element { name = "Loop"; attrs = []; childs = [
      Xml.Element { name = "LoopEnd"; attrs = ["Value", "20.0"]; childs = [Xml.Data ""] };
      Xml.Element { name = "StartRelative"; attrs = ["Value", "0"]; childs = [Xml.Data ""] };
      Xml.Element { name = "LoopOn"; attrs = ["Value", "false"]; childs = [Xml.Data ""] };
    ]} in
  try
    let _ = create xml in
    fail "Expected error for missing LoopStart"
  with
  | _ -> () (* Expected to fail - missing required attribute *)

let test_missing_loop_end () =
  let xml = Xml.Element { name = "Loop"; attrs = []; childs = [
      Xml.Element { name = "LoopStart"; attrs = ["Value", "10.0"]; childs = [Xml.Data ""] };
      Xml.Element { name = "StartRelative"; attrs = ["Value", "0"]; childs = [Xml.Data ""] };
      Xml.Element { name = "LoopOn"; attrs = ["Value", "false"]; childs = [Xml.Data ""] };
    ]} in
  try
    let _ = create xml in
    fail "Expected error for missing LoopEnd"
  with
  | _ -> () (* Expected to fail - missing required attribute *)

let test_diff_start_time_change () =
  let old_loop = { start_time = 10.0; end_time = 20.0; on = true; xml = dummy_xml } in
  let new_loop = { start_time = 15.0; end_time = 20.0; on = true; xml = dummy_xml } in
  let patch = diff old_loop new_loop in
  (match patch.start_time with
   | `Modified m ->
     check (float 0.001) "old start_time" 10.0 m.oldval;
     check (float 0.001) "new start_time" 15.0 m.newval
   | _ -> fail "Expected start_time to be modified")

let test_diff_end_time_change () =
  let old_loop = { start_time = 10.0; end_time = 20.0; on = true; xml = dummy_xml } in
  let new_loop = { start_time = 10.0; end_time = 25.0; on = true; xml = dummy_xml } in
  let patch = diff old_loop new_loop in
  (match patch.end_time with
   | `Modified m ->
     check (float 0.001) "old end_time" 20.0 m.oldval;
     check (float 0.001) "new end_time" 25.0 m.newval
   | _ -> fail "Expected end_time to be modified")

let test_diff_on_change () =
  let old_loop = { start_time = 10.0; end_time = 20.0; on = true; xml = dummy_xml } in
  let new_loop = { start_time = 10.0; end_time = 20.0; on = false; xml = dummy_xml } in
  let patch = diff old_loop new_loop in
  (match patch.on with
   | `Modified m ->
     check bool "old on" true m.oldval;
     check bool "new on" false m.newval
   | _ -> fail "Expected on to be modified")

let test_diff_all_change () =
  let old_loop = { start_time = 10.0; end_time = 20.0; on = true; xml = dummy_xml } in
  let new_loop = { start_time = 15.0; end_time = 25.0; on = false; xml = dummy_xml } in
  let patch = diff old_loop new_loop in
  (match patch.start_time with
   | `Modified m ->
     check (float 0.001) "old start_time" 10.0 m.oldval;
     check (float 0.001) "new start_time" 15.0 m.newval
   | _ -> fail "Expected start_time to be modified");
  (match patch.end_time with
   | `Modified m ->
     check (float 0.001) "old end_time" 20.0 m.oldval;
     check (float 0.001) "new end_time" 25.0 m.newval
   | _ -> fail "Expected end_time to be modified");
  (match patch.on with
   | `Modified m ->
     check bool "old on" true m.oldval;
     check bool "new on" false m.newval
   | _ -> fail "Expected on to be modified")

let test_diff_unchanged () =
  let loop = { start_time = 10.0; end_time = 20.0; on = true; xml = dummy_xml } in
  let patch = diff loop loop in
  check bool "patch is empty" true (Patch.is_empty patch)

let test_patch_is_empty_true () =
  let loop = { start_time = 10.0; end_time = 20.0; on = true; xml = dummy_xml } in
  let patch = diff loop loop in
  check bool "patch is empty" true (Patch.is_empty patch)

let test_patch_is_empty_false () =
  let old_loop = { start_time = 10.0; end_time = 20.0; on = true; xml = dummy_xml } in
  let new_loop = { start_time = 15.0; end_time = 20.0; on = true; xml = dummy_xml } in
  let patch = diff old_loop new_loop in
  check bool "patch is not empty" false (Patch.is_empty patch)

let () =
  run "Loop" [
    "parsing", [
      test_case "create basic loop" `Quick test_create_basic;
      test_case "create enabled loop" `Quick test_create_loop_enabled;
      test_case "create loop with different times" `Quick test_create_different_times;
      test_case "invalid element name raises error" `Quick test_invalid_element_name;
      test_case "missing LoopStart raises error" `Quick test_missing_loop_start;
      test_case "missing LoopEnd raises error" `Quick test_missing_loop_end;
    ];
    "diffing", [
      test_case "detect start_time changes" `Quick test_diff_start_time_change;
      test_case "detect end_time changes" `Quick test_diff_end_time_change;
      test_case "detect on changes" `Quick test_diff_on_change;
      test_case "detect all fields change" `Quick test_diff_all_change;
      test_case "unchanged loop produces empty patch" `Quick test_diff_unchanged;
    ];
    "patch", [
      test_case "patch is empty for unchanged values" `Quick test_patch_is_empty_true;
      test_case "patch is not empty for changed values" `Quick test_patch_is_empty_false;
    ];
  ]
