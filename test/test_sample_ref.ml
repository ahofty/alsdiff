open Alcotest
open Alsdiff_base
open Alsdiff_live.Clip.SampleRef
open Utils

let dummy_xml = Xml.read_string "<dummy/>"

let test_create_basic () =
  let xml = Xml.read_file (resolve_test_data_path "sample_ref.xml") in
  let sample_ref = create xml in
  check string "file_path" "/Users/krfantasy/Desktop/Prelude/Thick Air Project/Samples/Processed/Crop/Metal Sheet [2022-04-27 164454].wav" sample_ref.file_path;
  check string "crc" "48320" sample_ref.crc;
  check int "last_modified_date" 1742403845 sample_ref.last_modified_date

let test_create_different_file () =
  let xml = Xml.Element { name = "SampleRef"; attrs = []; childs = [
      Xml.Element { name = "FileRef"; attrs = []; childs = [
          Xml.Element { name = "RelativePathType"; attrs = ["Value", "3"]; childs = [Xml.Data ""] };
          Xml.Element { name = "RelativePath"; attrs = ["Value", "Samples/test.wav"]; childs = [Xml.Data ""] };
          Xml.Element { name = "Path"; attrs = ["Value", "/path/to/samples/test.wav"]; childs = [Xml.Data ""] };
          Xml.Element { name = "Type"; attrs = ["Value", "2"]; childs = [Xml.Data ""] };
          Xml.Element { name = "LivePackName"; attrs = ["Value", ""]; childs = [Xml.Data ""] };
          Xml.Element { name = "LivePackId"; attrs = ["Value", ""]; childs = [Xml.Data ""] };
          Xml.Element { name = "OriginalFileSize"; attrs = ["Value", "1000000"]; childs = [Xml.Data ""] };
          Xml.Element { name = "OriginalCrc"; attrs = ["Value", "12345"]; childs = [Xml.Data ""] };
        ]};
      Xml.Element { name = "LastModDate"; attrs = ["Value", "1742403846"]; childs = [Xml.Data ""] };
      Xml.Element { name = "SourceContext"; attrs = []; childs = [] };
      Xml.Element { name = "SampleUsageHint"; attrs = ["Value", "0"]; childs = [Xml.Data ""] };
      Xml.Element { name = "DefaultDuration"; attrs = ["Value", "1000000"]; childs = [Xml.Data ""] };
      Xml.Element { name = "DefaultSampleRate"; attrs = ["Value", "48000"]; childs = [Xml.Data ""] };
      Xml.Element { name = "SamplesToAutoWarp"; attrs = ["Value", "0"]; childs = [Xml.Data ""] };
    ]} in
  let sample_ref = create xml in
  check string "file_path" "/path/to/samples/test.wav" sample_ref.file_path;
  check string "crc" "12345" sample_ref.crc;
  check int "last_modified_date" 1742403846 sample_ref.last_modified_date

let test_invalid_element_name () =
  let xml = Xml.Element { name = "InvalidSampleRef"; attrs = []; childs = [] } in
  try
    let _ = create xml in
    fail "Expected Xml_error for invalid element name"
  with
  | Xml.Xml_error (_, msg) ->
    check string "error message" "Invalid XML element for creating SampleRef" msg

let test_missing_file_ref () =
  let xml = Xml.Element { name = "SampleRef"; attrs = []; childs = [
      Xml.Element { name = "LastModDate"; attrs = ["Value", "1742403845"]; childs = [Xml.Data ""] };
    ]} in
  try
    let _ = create xml in
    fail "Expected error for missing FileRef"
  with
  | _ -> () (* Expected to fail - missing required element *)

let test_missing_last_mod_date () =
  let xml = Xml.Element { name = "SampleRef"; attrs = []; childs = [
      Xml.Element { name = "FileRef"; attrs = []; childs = [
          Xml.Element { name = "Path"; attrs = ["Value", "/path/to/file.wav"]; childs = [Xml.Data ""] };
          Xml.Element { name = "OriginalCrc"; attrs = ["Value", "12345"]; childs = [Xml.Data ""] };
        ]};
    ]} in
  try
    let _ = create xml in
    fail "Expected error for missing LastModDate"
  with
  | _ -> () (* Expected to fail - missing required element *)

let test_diff_file_path_change () =
  let old_ref = { file_path = "/path/to/old.wav"; crc = "12345"; last_modified_date = 1742403845; xml = dummy_xml } in
  let new_ref = { file_path = "/path/to/new.wav"; crc = "12345"; last_modified_date = 1742403845; xml = dummy_xml } in
  let patch = diff old_ref new_ref in
  (match patch.file_path with
   | `Modified m ->
     check string "old file_path" "/path/to/old.wav" m.oldval;
     check string "new file_path" "/path/to/new.wav" m.newval
   | _ -> fail "Expected file_path to be modified")

let test_diff_crc_change () =
  let old_ref = { file_path = "/path/to/file.wav"; crc = "12345"; last_modified_date = 1742403845; xml = dummy_xml } in
  let new_ref = { file_path = "/path/to/file.wav"; crc = "67890"; last_modified_date = 1742403845; xml = dummy_xml } in
  let patch = diff old_ref new_ref in
  (match patch.crc with
   | `Modified m ->
     check string "old crc" "12345" m.oldval;
     check string "new crc" "67890" m.newval
   | _ -> fail "Expected crc to be modified")

let test_diff_last_modified_date_change () =
  let old_ref = { file_path = "/path/to/file.wav"; crc = "12345"; last_modified_date = 1742403845; xml = dummy_xml } in
  let new_ref = { file_path = "/path/to/file.wav"; crc = "12345"; last_modified_date = 1742403900; xml = dummy_xml } in
  let patch = diff old_ref new_ref in
  (match patch.last_modified_date with
   | `Modified m ->
     check int "old last_modified_date" 1742403845 m.oldval;
     check int "new last_modified_date" 1742403900 m.newval
   | _ -> fail "Expected last_modified_date to be modified")

let test_diff_all_change () =
  let old_ref = { file_path = "/path/to/old.wav"; crc = "12345"; last_modified_date = 1742403845; xml = dummy_xml } in
  let new_ref = { file_path = "/path/to/new.wav"; crc = "67890"; last_modified_date = 1742403900; xml = dummy_xml } in
  let patch = diff old_ref new_ref in
  (match patch.file_path with
   | `Modified m ->
     check string "old file_path" "/path/to/old.wav" m.oldval;
     check string "new file_path" "/path/to/new.wav" m.newval
   | _ -> fail "Expected file_path to be modified");
  (match patch.crc with
   | `Modified m ->
     check string "old crc" "12345" m.oldval;
     check string "new crc" "67890" m.newval
   | _ -> fail "Expected crc to be modified");
  (match patch.last_modified_date with
   | `Modified m ->
     check int "old last_modified_date" 1742403845 m.oldval;
     check int "new last_modified_date" 1742403900 m.newval
   | _ -> fail "Expected last_modified_date to be modified")

let test_diff_unchanged () =
  let ref = { file_path = "/path/to/file.wav"; crc = "12345"; last_modified_date = 1742403845; xml = dummy_xml } in
  let patch = diff ref ref in
  check bool "patch is empty" true (Patch.is_empty patch)

let test_patch_is_empty_true () =
  let ref = { file_path = "/path/to/file.wav"; crc = "12345"; last_modified_date = 1742403845; xml = dummy_xml } in
  let patch = diff ref ref in
  check bool "patch is empty" true (Patch.is_empty patch)

let test_patch_is_empty_false () =
  let old_ref = { file_path = "/path/to/old.wav"; crc = "12345"; last_modified_date = 1742403845; xml = dummy_xml } in
  let new_ref = { file_path = "/path/to/new.wav"; crc = "12345"; last_modified_date = 1742403845; xml = dummy_xml } in
  let patch = diff old_ref new_ref in
  check bool "patch is not empty" false (Patch.is_empty patch)

let () =
  run "SampleRef" [
    "parsing", [
      test_case "create basic sample ref" `Quick test_create_basic;
      test_case "create sample ref with different file" `Quick test_create_different_file;
      test_case "invalid element name raises error" `Quick test_invalid_element_name;
      test_case "missing FileRef raises error" `Quick test_missing_file_ref;
      test_case "missing LastModDate raises error" `Quick test_missing_last_mod_date;
    ];
    "diffing", [
      test_case "detect file_path changes" `Quick test_diff_file_path_change;
      test_case "detect crc changes" `Quick test_diff_crc_change;
      test_case "detect last_modified_date changes" `Quick test_diff_last_modified_date_change;
      test_case "detect all fields change" `Quick test_diff_all_change;
      test_case "unchanged sample ref produces empty patch" `Quick test_diff_unchanged;
    ];
    "patch", [
      test_case "patch is empty for unchanged values" `Quick test_patch_is_empty_true;
      test_case "patch is not empty for changed values" `Quick test_patch_is_empty_false;
    ];
  ]
