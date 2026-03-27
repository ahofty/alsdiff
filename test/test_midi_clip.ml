open Alcotest
open Alsdiff_base
open Alsdiff_live.Clip
open Utils

let test_midi_clip_parsing () =
  (* Read the midi_clip.xml file *)
  let xml = Xml.read_file (resolve_test_data_path "midi_clip.xml") in

  (* Create midi clip from the XML *)
  let midi_clip = MidiClip.create xml in

  (* Expected values based on the XML file *)
  let expected_id = 2 in
  let expected_start_time = 80.0 in
  let expected_end_time = 100.0 in
  let expected_signature = { TimeSignature.numer = 4; TimeSignature.denom = 4 } in
  let expected_loop = Some {
      Loop.start_time = 92.0;
      Loop.end_time = 112.0;
      Loop.on = false;
    } in

  (* Test basic fields *)
  check int "id" expected_id midi_clip.id;
  check (float 0.001) "start_time" expected_start_time midi_clip.start_time;
  check (float 0.001) "end_time" expected_end_time midi_clip.end_time;
  check (pair int int) "signature" (expected_signature.numer, expected_signature.denom) (midi_clip.signature.numer, midi_clip.signature.denom);

  (* Test loop section *)
  match expected_loop with
  | Some expected_loop ->
    check (float 0.001) "loop.start_time" expected_loop.Loop.start_time midi_clip.loop.start_time;
    check (float 0.001) "loop.end_time" expected_loop.Loop.end_time midi_clip.loop.end_time;
    check bool "loop.on" expected_loop.Loop.on midi_clip.loop.on
  | None -> fail "Expected expected_loop to be Some"

let test_notes_list_extraction () =
  let xml = Xml.read_file (resolve_test_data_path "midi_clip.xml") in
  let midi_clip = MidiClip.create xml in

  (* The midi_clip.xml has 19 notes across 5 KeyTracks *)
  check bool "has notes" true (List.length midi_clip.notes > 0);
  check int "notes count" 19 (List.length midi_clip.notes);

  (* Check that notes have correct structure *)
  let first_note = List.hd midi_clip.notes in
  check int "first_note.id" 1 first_note.MidiNote.id;
  check int "first_note.note" 60 first_note.MidiNote.note;
  check (float 0.001) "first_note.time" 3.0 first_note.MidiNote.time

let test_notes_from_multiple_keys () =
  let xml = Xml.read_file (resolve_test_data_path "midi_clip.xml") in
  let midi_clip = MidiClip.create xml in

  (* Notes should come from different MIDI keys (60, 62, 64, 65, 67) *)
  let unique_keys = midi_clip.notes
    |> List.map (fun n -> n.MidiNote.note)
    |> List.sort_uniq Int.compare in
  check bool "has multiple keys" true (List.length unique_keys > 1);
  check int "unique keys count" 5 (List.length unique_keys)

let test_notes_ordered_by_time () =
  let xml = Xml.read_file (resolve_test_data_path "midi_clip.xml") in
  let midi_clip = MidiClip.create xml in

  (* Check that notes are ordered by time within each key *)
  let notes_by_key_60 = midi_clip.notes
    |> List.filter (fun n -> n.MidiNote.note = 60) in
  check int "notes for key 60" 5 (List.length notes_by_key_60);

  (* Verify time ordering for key 60 notes *)
  let times = List.map (fun n -> n.MidiNote.time) notes_by_key_60 in
  let sorted_times = List.sort Float.compare times in
  check bool "notes ordered by time" true (times = sorted_times)

let test_notes_have_valid_ids () =
  let xml = Xml.read_file (resolve_test_data_path "midi_clip.xml") in
  let midi_clip = MidiClip.create xml in

  (* All notes should have unique positive IDs *)
  let ids = midi_clip.notes |> List.map (fun n -> n.MidiNote.id) in
  let unique_ids = List.sort_uniq Int.compare ids in
  check bool "all ids unique" true (List.length ids = List.length unique_ids);
  check bool "all ids positive" true (List.for_all (fun id -> id > 0) ids)

let test_notes_velocity_range () =
  let xml = Xml.read_file (resolve_test_data_path "midi_clip.xml") in
  let midi_clip = MidiClip.create xml in

  (* Check that all notes have valid velocity values (0-127) *)
  let all_valid_velocity = midi_clip.notes
    |> List.for_all (fun n -> n.MidiNote.velocity >= 0.0 && n.MidiNote.velocity <= 127.0) in
  check bool "all velocities valid" true all_valid_velocity

(* Derived function tests *)

(* Helper function to create a basic clip for testing *)
let create_basic_clip () =
  {
    MidiClip.id = 1;
    name = "Test Clip";
    start_time = 0.0;
    end_time = 4.0;
    loop = { Loop.start_time = 0.0; end_time = 4.0; on = false; };
    signature = { TimeSignature.numer = 4; denom = 4; };
    notes = [];
  }

(* Diff function tests *)
let test_diff_name_change () =
  let old_clip = create_basic_clip () in
  let new_clip = { old_clip with name = "New Name" } in
  let patch = MidiClip.diff old_clip new_clip in
  match patch.MidiClip.Patch.name with
  | `Modified {oldval; newval} ->
    check string "old name" "Test Clip" oldval;
    check string "new name" "New Name" newval
  | _ -> fail "Expected name to be `Modified"

let test_diff_start_time_change () =
  let old_clip = create_basic_clip () in
  let new_clip = { old_clip with start_time = 1.5 } in
  let patch = MidiClip.diff old_clip new_clip in
  match patch.MidiClip.Patch.start_time with
  | `Modified {oldval; newval} ->
    check (float 0.001) "old start_time" 0.0 oldval;
    check (float 0.001) "new start_time" 1.5 newval
  | _ -> fail "Expected start_time to be `Modified"

let test_diff_end_time_change () =
  let old_clip = create_basic_clip () in
  let new_clip = { old_clip with end_time = 8.0 } in
  let patch = MidiClip.diff old_clip new_clip in
  match patch.MidiClip.Patch.end_time with
  | `Modified {oldval; newval} ->
    check (float 0.001) "old end_time" 4.0 oldval;
    check (float 0.001) "new end_time" 8.0 newval
  | _ -> fail "Expected end_time to be `Modified"

let test_diff_loop_change () =
  let old_clip = create_basic_clip () in
  let new_loop = { Loop.start_time = 1.0; end_time = 3.0; on = true; } in
  let new_clip = { old_clip with loop = new_loop } in
  let patch = MidiClip.diff old_clip new_clip in
  match patch.MidiClip.Patch.loop with
  | `Modified loop_patch ->
    check bool "loop changed" true (not (Loop.Patch.is_empty loop_patch))
  | `Unchanged -> fail "Expected loop to be `Modified"

let test_diff_signature_change () =
  let old_clip = create_basic_clip () in
  let new_sig = { TimeSignature.numer = 3; denom = 4; } in
  let new_clip = { old_clip with signature = new_sig } in
  let patch = MidiClip.diff old_clip new_clip in
  match patch.MidiClip.Patch.signature with
  | `Modified sig_patch ->
    check bool "signature changed" true (not (TimeSignature.Patch.is_empty sig_patch))
  | `Unchanged -> fail "Expected signature to be `Modified"

let test_diff_notes_change () =
  let old_clip = create_basic_clip () in
  let new_note = { MidiNote.id = 1; note = 60; time = 0.0; duration = 1.0; velocity = 100.0; off_velocity = 0.0; } in
  let new_clip = { old_clip with notes = [new_note] } in
  let patch = MidiClip.diff old_clip new_clip in
  (* notes diff is a list of changes *)
  check bool "notes changed" true (patch.MidiClip.Patch.notes <> [])

let test_diff_multiple_changes () =
  let old_clip = create_basic_clip () in
  let new_clip = { old_clip with name = "New Name"; start_time = 1.0; end_time = 5.0; } in
  let patch = MidiClip.diff old_clip new_clip in
  check bool "name changed" true (match patch.MidiClip.Patch.name with `Modified _ -> true | _ -> false);
  check bool "start_time changed" true (match patch.MidiClip.Patch.start_time with `Modified _ -> true | _ -> false);
  check bool "end_time changed" true (match patch.MidiClip.Patch.end_time with `Modified _ -> true | _ -> false)

let test_diff_no_change () =
  let clip = create_basic_clip () in
  let patch = MidiClip.diff clip clip in
  check bool "patch empty" true (MidiClip.Patch.is_empty patch)

(* ID function tests *)
let test_has_same_id_same () =
  let clip = create_basic_clip () in
  check bool "same clip" true (MidiClip.has_same_id clip clip)

let test_has_same_id_different () =
  let clip1 = { (create_basic_clip ()) with MidiClip.id = 1 } in
  let clip2 = { (create_basic_clip ()) with MidiClip.id = 2 } in
  check bool "different clips" false (MidiClip.has_same_id clip1 clip2)

let test_id_hash_consistent () =
  let clip = { (create_basic_clip ()) with MidiClip.id = 42 } in
  let hash1 = MidiClip.id_hash clip in
  let hash2 = MidiClip.id_hash clip in
  check bool "same id same hash" true (hash1 = hash2)

(* Patch.is_empty tests *)
let test_patch_empty_no_changes () =
  let clip = create_basic_clip () in
  let patch = MidiClip.diff clip clip in
  check bool "patch empty" true (MidiClip.Patch.is_empty patch)

let test_patch_not_empty_with_changes () =
  let old_clip = create_basic_clip () in
  let new_clip = { old_clip with name = "Different" } in
  let patch = MidiClip.diff old_clip new_clip in
  check bool "patch not empty" false (MidiClip.Patch.is_empty patch)

let () =
  run "MidiClip" [
    "parsing", [
      test_case "parse MidiClip XML" `Quick test_midi_clip_parsing;
      test_case "extract notes list" `Quick test_notes_list_extraction;
      test_case "notes from multiple keys" `Quick test_notes_from_multiple_keys;
      test_case "notes ordered by time" `Quick test_notes_ordered_by_time;
      test_case "notes have valid ids" `Quick test_notes_have_valid_ids;
      test_case "notes velocity range" `Quick test_notes_velocity_range;
    ];
    "diffing", [
      test_case "detect name changes" `Quick test_diff_name_change;
      test_case "detect start_time changes" `Quick test_diff_start_time_change;
      test_case "detect end_time changes" `Quick test_diff_end_time_change;
      test_case "detect loop changes" `Quick test_diff_loop_change;
      test_case "detect signature changes" `Quick test_diff_signature_change;
      test_case "detect notes changes" `Quick test_diff_notes_change;
      test_case "detect multiple changes" `Quick test_diff_multiple_changes;
      test_case "unchanged clip produces empty patch" `Quick test_diff_no_change;
    ];
    "id_functions", [
      test_case "has_same_id same clip" `Quick test_has_same_id_same;
      test_case "has_same_id different clips" `Quick test_has_same_id_different;
      test_case "id_hash consistent" `Quick test_id_hash_consistent;
    ];
    "patch", [
      test_case "patch empty when no changes" `Quick test_patch_empty_no_changes;
      test_case "patch not empty with changes" `Quick test_patch_not_empty_with_changes;
    ];
  ]
