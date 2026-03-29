open Alcotest
open Alsdiff_base
open Alsdiff_live.Clip.TimeSignature
open Utils

let test_create_basic_44 () =
  let xml = Xml.read_file (resolve_test_data_path "time_signature.xml") in
  let sig_val = create xml in
  check int "numer" 4 sig_val.numer;
  check int "denom" 4 sig_val.denom

let test_create_34_time () =
  let xml = Xml.Element { name = "RemoteableTimeSignature"; attrs = ["Id", "0"]; childs = [
      Xml.Element { name = "Numerator"; attrs = ["Value", "3"]; childs = [Xml.Data ""] };
      Xml.Element { name = "Denominator"; attrs = ["Value", "4"]; childs = [Xml.Data ""] };
      Xml.Element { name = "Time"; attrs = ["Value", "0"]; childs = [Xml.Data ""] };
    ]} in
  let sig_val = create xml in
  check int "numer" 3 sig_val.numer;
  check int "denom" 4 sig_val.denom

let test_create_68_time () =
  let xml = Xml.Element { name = "RemoteableTimeSignature"; attrs = ["Id", "0"]; childs = [
      Xml.Element { name = "Numerator"; attrs = ["Value", "6"]; childs = [Xml.Data ""] };
      Xml.Element { name = "Denominator"; attrs = ["Value", "8"]; childs = [Xml.Data ""] };
      Xml.Element { name = "Time"; attrs = ["Value", "0"]; childs = [Xml.Data ""] };
    ]} in
  let sig_val = create xml in
  check int "numer" 6 sig_val.numer;
  check int "denom" 8 sig_val.denom

let test_invalid_element_name () =
  let xml = Xml.Element { name = "InvalidTimeSignature"; attrs = []; childs = [] } in
  try
    let _ = create xml in
    fail "Expected Xml_error for invalid element name"
  with
  | Xml.Xml_error (_, msg) ->
    check string "error message" "Invalid XML element for creating TimeSignature" msg

let test_missing_numerator () =
  let xml = Xml.Element { name = "RemoteableTimeSignature"; attrs = ["Id", "0"]; childs = [
      Xml.Element { name = "Denominator"; attrs = ["Value", "4"]; childs = [Xml.Data ""] };
      Xml.Element { name = "Time"; attrs = ["Value", "0"]; childs = [Xml.Data ""] };
    ]} in
  try
    let _ = create xml in
    fail "Expected Xml_error for missing Numerator"
  with
  | Upath.Path_not_found _ -> ()

let test_missing_denominator () =
  let xml = Xml.Element { name = "RemoteableTimeSignature"; attrs = ["Id", "0"]; childs = [
      Xml.Element { name = "Numerator"; attrs = ["Value", "4"]; childs = [Xml.Data ""] };
      Xml.Element { name = "Time"; attrs = ["Value", "0"]; childs = [Xml.Data ""] };
    ]} in
  try
    let _ = create xml in
    fail "Expected Xml_error for missing Denominator"
  with
  | Upath.Path_not_found _ -> ()

let test_diff_numer_change () =
  let old_sig = { numer = 4; denom = 4 } in
  let new_sig = { numer = 3; denom = 4 } in
  let patch = diff old_sig new_sig in
  (match patch.numer with
   | `Modified m ->
     check int "old numer" 4 m.oldval;
     check int "new numer" 3 m.newval
   | _ -> fail "Expected numer to be modified")

let test_diff_denom_change () =
  let old_sig = { numer = 4; denom = 4 } in
  let new_sig = { numer = 4; denom = 8 } in
  let patch = diff old_sig new_sig in
  (match patch.denom with
   | `Modified m ->
     check int "old denom" 4 m.oldval;
     check int "new denom" 8 m.newval
   | _ -> fail "Expected denom to be modified")

let test_diff_both_change () =
  let old_sig = { numer = 3; denom = 4 } in
  let new_sig = { numer = 6; denom = 8 } in
  let patch = diff old_sig new_sig in
  (match patch.numer with
   | `Modified m ->
     check int "old numer" 3 m.oldval;
     check int "new numer" 6 m.newval
   | _ -> fail "Expected numer to be modified");
  (match patch.denom with
   | `Modified m ->
     check int "old denom" 4 m.oldval;
     check int "new denom" 8 m.newval
   | _ -> fail "Expected denom to be modified")

let test_diff_unchanged () =
  let sig_val = { numer = 4; denom = 4 } in
  let patch = diff sig_val sig_val in
  check bool "patch is empty" true (Patch.is_empty patch)

let test_patch_is_empty_true () =
  let sig_val = { numer = 4; denom = 4 } in
  let patch = diff sig_val sig_val in
  check bool "patch is empty" true (Patch.is_empty patch)

let test_patch_is_empty_false () =
  let old_sig = { numer = 4; denom = 4 } in
  let new_sig = { numer = 3; denom = 4 } in
  let patch = diff old_sig new_sig in
  check bool "patch is not empty" false (Patch.is_empty patch)

let () =
  run "TimeSignature" [
    "parsing", [
      test_case "create 4/4 time signature" `Quick test_create_basic_44;
      test_case "create 3/4 time signature" `Quick test_create_34_time;
      test_case "create 6/8 time signature" `Quick test_create_68_time;
      test_case "invalid element name raises error" `Quick test_invalid_element_name;
      test_case "missing Numerator raises error" `Quick test_missing_numerator;
      test_case "missing Denominator raises error" `Quick test_missing_denominator;
    ];
    "diffing", [
      test_case "detect numerator changes" `Quick test_diff_numer_change;
      test_case "detect denominator changes" `Quick test_diff_denom_change;
      test_case "detect both numerator and denominator changes" `Quick test_diff_both_change;
      test_case "unchanged signature produces empty patch" `Quick test_diff_unchanged;
    ];
    "patch", [
      test_case "patch is empty for unchanged values" `Quick test_patch_is_empty_true;
      test_case "patch is not empty for changed values" `Quick test_patch_is_empty_false;
    ];
  ]
