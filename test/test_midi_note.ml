open Alcotest
open Alsdiff_base
open Alsdiff_live.Clip.MidiNote
open Utils

let test_create_basic () =
  let xml = Xml.read_file (resolve_test_data_path "midi_note.xml") in
  let note = create 60 xml in
  check int "id" 1 note.id;
  check int "note" 60 note.note;
  check (float 0.001) "time" 3.0 note.time;
  check (float 0.001) "duration" 0.457430850399600408 note.duration;
  check (float 0.001) "velocity" 100.0 note.velocity;
  check (float 0.001) "off_velocity" 64.0 note.off_velocity

let test_create_from_xml_element () =
  let xml = Xml.Element { name = "MidiNoteEvent"; attrs = ["NoteId", "42"; "Time", "1.5"; "Duration", "0.5"; "Velocity", "80"; "OffVelocity", "50"]; childs = [] } in
  let note = create 64 xml in
  check int "id" 42 note.id;
  check int "note" 64 note.note;
  check (float 0.001) "time" 1.5 note.time;
  check (float 0.001) "duration" 0.5 note.duration;
  check (float 0.001) "velocity" 80.0 note.velocity;
  check (float 0.001) "off_velocity" 50.0 note.off_velocity

let test_invalid_element_name () =
  let xml = Xml.Element { name = "InvalidElement"; attrs = []; childs = [] } in
  try
    let _ = create 60 xml in
    fail "Expected Xml_error for invalid element name"
  with
  | Xml.Xml_error (_, msg) ->
    check string "error message" "Invalid XML element for creating MidiNote" msg

let test_missing_note_id () =
  let xml = Xml.Element { name = "MidiNoteEvent"; attrs = ["Time", "1.0"; "Duration", "0.5"; "Velocity", "100"; "OffVelocity", "64"]; childs = [] } in
  try
    let _ = create 60 xml in
    fail "Expected error for missing NoteId"
  with
  | _ -> () (* Expected to fail - missing required attribute *)

let test_has_same_id () =
  let note1 = { id = 1; note = 60; time = 1.0; duration = 0.5; velocity = 100.0; off_velocity = 64.0 } in
  let note2 = { id = 1; note = 62; time = 2.0; duration = 1.0; velocity = 80.0; off_velocity = 50.0 } in
  let note3 = { id = 2; note = 60; time = 1.0; duration = 0.5; velocity = 100.0; off_velocity = 64.0 } in
  check bool "same id" true (has_same_id note1 note2);
  check bool "different id" false (has_same_id note1 note3)

let test_id_hash () =
  let note1 = { id = 1; note = 60; time = 1.0; duration = 0.5; velocity = 100.0; off_velocity = 64.0 } in
  let note2 = { id = 1; note = 62; time = 2.0; duration = 1.0; velocity = 80.0; off_velocity = 50.0 } in
  let hash1 = id_hash note1 in
  let hash2 = id_hash note2 in
  check bool "same id produces same hash" true (hash1 = hash2)

let test_diff_time_change () =
  let old_note = { id = 1; note = 60; time = 1.0; duration = 0.5; velocity = 100.0; off_velocity = 64.0 } in
  let new_note = { id = 1; note = 60; time = 2.0; duration = 0.5; velocity = 100.0; off_velocity = 64.0 } in
  let patch = diff old_note new_note in
  (match patch.time with
   | `Modified m ->
     check (float 0.001) "old time" 1.0 m.oldval;
     check (float 0.001) "new time" 2.0 m.newval
   | _ -> fail "Expected time to be modified")

let test_diff_velocity_change () =
  let old_note = { id = 1; note = 60; time = 1.0; duration = 0.5; velocity = 100.0; off_velocity = 64.0 } in
  let new_note = { id = 1; note = 60; time = 1.0; duration = 0.5; velocity = 80.0; off_velocity = 64.0 } in
  let patch = diff old_note new_note in
  (match patch.velocity with
   | `Modified m ->
     check (float 0.001) "old velocity" 100.0 m.oldval;
     check (float 0.001) "new velocity" 80.0 m.newval
   | _ -> fail "Expected velocity to be modified")

let test_diff_unchanged () =
  let note = { id = 1; note = 60; time = 1.0; duration = 0.5; velocity = 100.0; off_velocity = 64.0 } in
  let patch = diff note note in
  check bool "patch is empty" true (Patch.is_empty patch)

let test_patch_is_empty_true () =
  let note = { id = 1; note = 60; time = 1.0; duration = 0.5; velocity = 100.0; off_velocity = 64.0 } in
  let patch = diff note note in
  check bool "patch is empty" true (Patch.is_empty patch)

let test_patch_is_empty_false () =
  let old_note = { id = 1; note = 60; time = 1.0; duration = 0.5; velocity = 100.0; off_velocity = 64.0 } in
  let new_note = { id = 1; note = 60; time = 2.0; duration = 0.5; velocity = 100.0; off_velocity = 64.0 } in
  let patch = diff old_note new_note in
  check bool "patch is not empty" false (Patch.is_empty patch)

let test_diff_note_value_change () =
  let old_note = { id = 1; note = 60; time = 1.0; duration = 0.5; velocity = 100.0; off_velocity = 64.0 } in
  let new_note = { id = 1; note = 64; time = 1.0; duration = 0.5; velocity = 100.0; off_velocity = 64.0 } in
  let patch = diff old_note new_note in
  (match patch.note with
   | `Modified m ->
     check int "old note" 60 m.oldval;
     check int "new note" 64 m.newval
   | _ -> fail "Expected note to be modified")

let test_diff_duration_change () =
  let old_note = { id = 1; note = 60; time = 1.0; duration = 0.5; velocity = 100.0; off_velocity = 64.0 } in
  let new_note = { id = 1; note = 60; time = 1.0; duration = 1.5; velocity = 100.0; off_velocity = 64.0 } in
  let patch = diff old_note new_note in
  (match patch.duration with
   | `Modified m ->
     check (float 0.001) "old duration" 0.5 m.oldval;
     check (float 0.001) "new duration" 1.5 m.newval
   | _ -> fail "Expected duration to be modified")

let test_diff_off_velocity_change () =
  let old_note = { id = 1; note = 60; time = 1.0; duration = 0.5; velocity = 100.0; off_velocity = 64.0 } in
  let new_note = { id = 1; note = 60; time = 1.0; duration = 0.5; velocity = 100.0; off_velocity = 32.0 } in
  let patch = diff old_note new_note in
  (match patch.off_velocity with
   | `Modified m ->
     check (float 0.001) "old off_velocity" 64.0 m.oldval;
     check (float 0.001) "new off_velocity" 32.0 m.newval
   | _ -> fail "Expected off_velocity to be modified")

let () =
  run "MidiNote" [
    "parsing", [
      test_case "create from valid XML" `Quick test_create_basic;
      test_case "create from XML element" `Quick test_create_from_xml_element;
      test_case "invalid element name raises error" `Quick test_invalid_element_name;
      test_case "missing NoteId raises error" `Quick test_missing_note_id;
    ];
    "id_functions", [
      test_case "has_same_id works" `Quick test_has_same_id;
      test_case "id_hash consistent" `Quick test_id_hash;
    ];
    "diffing", [
      test_case "detect time changes" `Quick test_diff_time_change;
      test_case "detect velocity changes" `Quick test_diff_velocity_change;
      test_case "detect note value changes" `Quick test_diff_note_value_change;
      test_case "detect duration changes" `Quick test_diff_duration_change;
      test_case "detect off_velocity changes" `Quick test_diff_off_velocity_change;
      test_case "unchanged note produces empty patch" `Quick test_diff_unchanged;
    ];
    "patch", [
      test_case "patch is empty for unchanged values" `Quick test_patch_is_empty_true;
      test_case "patch is not empty for changed values" `Quick test_patch_is_empty_false;
    ];
  ]
