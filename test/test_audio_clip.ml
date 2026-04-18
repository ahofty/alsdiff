open Alcotest
open Alsdiff_base
open Alsdiff_live.Clip
open Utils

let dummy_xml = Xml.read_string "<dummy/>"

let test_create_audio_clip () =
  (* Read the audio_clip.xml file *)
  let xml = Xml.read_file (resolve_test_data_path "audio_clip.xml") in

  (* Create audio clip from the XML *)
  let audio_clip = AudioClip.create xml in

  (* Expected values based on the XML file *)
  let expected_id = 17 in
  let expected_name = "Metal Sheet" in
  let expected_start_time = 79.5 in
  let expected_end_time = 100.0 in
  let expected_signature = { TimeSignature.numer = 4; denom = 4; xml = dummy_xml } in
  let expected_sample_ref = {
    SampleRef.file_path = "/Users/krfantasy/Desktop/Prelude/Thick Air Project/Samples/Processed/Crop/Metal Sheet [2022-04-27 164454].wav";
    SampleRef.crc = "48320";
    SampleRef.last_modified_date = 1742403845;
    xml = dummy_xml;
  } in
  let expected_loop = Some {
      Loop.start_time = 26.13179997086247;
      Loop.end_time = 46.631799970862474;
      Loop.on = false;
      xml = dummy_xml;
    } in

  (* Test basic fields *)
  check int "id" expected_id audio_clip.id;
  check string "name" expected_name audio_clip.name;
  check (float 0.001) "start_time" expected_start_time audio_clip.start_time;
  check (float 0.001) "end_time" expected_end_time audio_clip.end_time;
  check (pair int int) "signature" (expected_signature.numer, expected_signature.denom) (audio_clip.signature.numer, audio_clip.signature.denom);

  (* Test sample reference *)
  check string "sample_ref.file_path" expected_sample_ref.file_path audio_clip.sample_ref.file_path;
  check string "sample_ref.crc" expected_sample_ref.crc audio_clip.sample_ref.crc;
  check int "sample_ref.last_modified_date" expected_sample_ref.last_modified_date audio_clip.sample_ref.last_modified_date;

  (* Test loop section *)
  match expected_loop with
  | Some expected_loop ->
    check (float 0.001) "loop.start_time" expected_loop.Loop.start_time audio_clip.loop.start_time;
    check (float 0.001) "loop.end_time" expected_loop.Loop.end_time audio_clip.loop.end_time;
    check bool "loop.on" expected_loop.Loop.on audio_clip.loop.on
  | None -> fail "Expected expected_loop to be Some"

let test_fade_enabled () =
  (* audio_clip.xml has Fade Value="true" and contains Fades element *)
  let xml = Xml.read_file (resolve_test_data_path "audio_clip.xml") in
  let audio_clip = AudioClip.create xml in

  (* Check that fade is Some (enabled) *)
  match audio_clip.fade with
  | Some fade ->
    check (float 0.001) "fade.fade_in_length" 0.0 fade.Fade.fade_in_length;
    check (float 0.001) "fade.fade_out_length" 8.4680920641858144 fade.Fade.fade_out_length;
    check bool "fade.is_initialized" true fade.Fade.is_initialized
  | None -> fail "Expected fade to be Some when Fade Value=\"true\""

let test_fade_all_fields () =
  let xml = Xml.read_file (resolve_test_data_path "audio_clip.xml") in
  let audio_clip = AudioClip.create xml in

  match audio_clip.fade with
  | Some fade ->
    (* Check all 10 fade fields *)
    check (float 0.001) "fade_in_length" 0.0 fade.Fade.fade_in_length;
    check (float 0.001) "fade_out_length" 8.4680920641858144 fade.Fade.fade_out_length;
    check bool "is_initialized" true fade.Fade.is_initialized;
    check int "crossfade_state" 1 fade.Fade.crossfade_state;
    check (float 0.001) "fade_in_curve_skew" 0.0 fade.Fade.fade_in_curve_skew;
    check (float 0.001) "fade_in_curve_slope" 0.0 fade.Fade.fade_in_curve_slope;
    check (float 0.001) "fade_out_curve_skew" 0.0 fade.Fade.fade_out_curve_skew;
    check (float 0.001) "fade_out_curve_slope" 0.0 fade.Fade.fade_out_curve_slope;
    check bool "is_default_fade_in" false fade.Fade.is_default_fade_in;
    check bool "is_default_fade_out" false fade.Fade.is_default_fade_out
  | None -> fail "Expected fade to be Some"

let test_sample_ref_fields () =
  let xml = Xml.read_file (resolve_test_data_path "audio_clip.xml") in
  let audio_clip = AudioClip.create xml in

  (* Check all SampleRef fields *)
  check bool "file_path not empty" true (String.length audio_clip.sample_ref.SampleRef.file_path > 0);
  check bool "crc not empty" true (String.length audio_clip.sample_ref.SampleRef.crc > 0);
  check bool "last_modified_date positive" true (audio_clip.sample_ref.SampleRef.last_modified_date > 0)

let test_signature_fields () =
  let xml = Xml.read_file (resolve_test_data_path "audio_clip.xml") in
  let audio_clip = AudioClip.create xml in

  (* Check TimeSignature fields *)
  check bool "numer positive" true (audio_clip.signature.TimeSignature.numer > 0);
  check bool "denom positive" true (audio_clip.signature.TimeSignature.denom > 0)

let test_loop_fields () =
  let xml = Xml.read_file (resolve_test_data_path "audio_clip.xml") in
  let audio_clip = AudioClip.create xml in

  (* Check Loop fields *)
  check bool "start_time non_negative" true (audio_clip.loop.Loop.start_time >= 0.0);
  check bool "end_time >= start_time" true (audio_clip.loop.Loop.end_time >= audio_clip.loop.Loop.start_time)

(* Derived function tests *)

(* Helper function to create a basic clip for testing *)
let create_basic_clip () =
  {
    AudioClip.id = 1;
    name = "Test Clip";
    start_time = 0.0;
    end_time = 4.0;
    loop = { Loop.start_time = 0.0; end_time = 4.0; on = false; xml = dummy_xml; };
    signature = { TimeSignature.numer = 4; denom = 4; xml = dummy_xml; };
    sample_ref = { SampleRef.file_path = "/test/sample.wav"; crc = "12345"; last_modified_date = 1000000; xml = dummy_xml; };
    fade = None;
    xml = dummy_xml;
  }

(* Diff function tests *)
let test_diff_name_change () =
  let old_clip = create_basic_clip () in
  let new_clip = { old_clip with name = "New Name" } in
  let patch = AudioClip.diff old_clip new_clip in
  match patch.AudioClip.Patch.name with
  | `Modified {oldval; newval} ->
    check string "old name" "Test Clip" oldval;
    check string "new name" "New Name" newval
  | _ -> fail "Expected name to be `Modified"

let test_diff_start_time_change () =
  let old_clip = create_basic_clip () in
  let new_clip = { old_clip with start_time = 1.5 } in
  let patch = AudioClip.diff old_clip new_clip in
  match patch.AudioClip.Patch.start_time with
  | `Modified {oldval; newval} ->
    check (float 0.001) "old start_time" 0.0 oldval;
    check (float 0.001) "new start_time" 1.5 newval
  | _ -> fail "Expected start_time to be `Modified"

let test_diff_end_time_change () =
  let old_clip = create_basic_clip () in
  let new_clip = { old_clip with end_time = 8.0 } in
  let patch = AudioClip.diff old_clip new_clip in
  match patch.AudioClip.Patch.end_time with
  | `Modified {oldval; newval} ->
    check (float 0.001) "old end_time" 4.0 oldval;
    check (float 0.001) "new end_time" 8.0 newval
  | _ -> fail "Expected end_time to be `Modified"

let test_diff_loop_change () =
  let old_clip = create_basic_clip () in
  let new_loop = { Loop.start_time = 1.0; end_time = 3.0; on = true; xml = dummy_xml; } in
  let new_clip = { old_clip with loop = new_loop } in
  let patch = AudioClip.diff old_clip new_clip in
  match patch.AudioClip.Patch.loop with
  | `Modified loop_patch ->
    check bool "loop changed" true (not (Loop.Patch.is_empty loop_patch))
  | `Unchanged -> fail "Expected loop to be `Modified"

let test_diff_signature_change () =
  let old_clip = create_basic_clip () in
  let new_sig = { TimeSignature.numer = 3; denom = 4; xml = dummy_xml; } in
  let new_clip = { old_clip with signature = new_sig } in
  let patch = AudioClip.diff old_clip new_clip in
  match patch.AudioClip.Patch.signature with
  | `Modified sig_patch ->
    check bool "signature changed" true (not (TimeSignature.Patch.is_empty sig_patch))
  | `Unchanged -> fail "Expected signature to be `Modified"

let test_diff_sample_ref_change () =
  let old_clip = create_basic_clip () in
  let new_ref = { SampleRef.file_path = "/test/other.wav"; crc = "67890"; last_modified_date = 2000000; xml = dummy_xml; } in
  let new_clip = { old_clip with sample_ref = new_ref } in
  let patch = AudioClip.diff old_clip new_clip in
  match patch.AudioClip.Patch.sample_ref with
  | `Modified ref_patch ->
    check bool "sample_ref changed" true (not (SampleRef.Patch.is_empty ref_patch))
  | `Unchanged -> fail "Expected sample_ref to be `Modified"

(* Fade option diff tests *)
let test_diff_fade_none_to_some () =
  let old_clip = create_basic_clip () in
  let new_fade = { Fade.fade_in_length = 0.5; fade_out_length = 1.0; is_initialized = true; crossfade_state = 0; fade_in_curve_skew = 0.0; fade_in_curve_slope = 0.0; fade_out_curve_skew = 0.0; fade_out_curve_slope = 0.0; is_default_fade_in = false; is_default_fade_out = false; xml = dummy_xml; } in
  let new_clip = { old_clip with fade = Some new_fade } in
  let patch = AudioClip.diff old_clip new_clip in
  match patch.AudioClip.Patch.fade with
  | `Added _ -> check bool "fade added" true true
  | _ -> fail "Expected fade to be `Added"

let test_diff_fade_some_to_none () =
  let old_fade = { Fade.fade_in_length = 0.5; fade_out_length = 1.0; is_initialized = true; crossfade_state = 0; fade_in_curve_skew = 0.0; fade_in_curve_slope = 0.0; fade_out_curve_skew = 0.0; fade_out_curve_slope = 0.0; is_default_fade_in = false; is_default_fade_out = false; xml = dummy_xml; } in
  let old_clip = { (create_basic_clip ()) with fade = Some old_fade } in
  let new_clip = { old_clip with fade = None } in
  let patch = AudioClip.diff old_clip new_clip in
  match patch.AudioClip.Patch.fade with
  | `Removed _ -> check bool "fade removed" true true
  | _ -> fail "Expected fade to be `Removed"

let test_diff_fade_some_to_some () =
  let old_fade = { Fade.fade_in_length = 0.5; fade_out_length = 1.0; is_initialized = true; crossfade_state = 0; fade_in_curve_skew = 0.0; fade_in_curve_slope = 0.0; fade_out_curve_skew = 0.0; fade_out_curve_slope = 0.0; is_default_fade_in = false; is_default_fade_out = false; xml = dummy_xml; } in
  let new_fade = { old_fade with Fade.fade_out_length = 2.0 } in
  let old_clip = { (create_basic_clip ()) with fade = Some old_fade } in
  let new_clip = { old_clip with fade = Some new_fade } in
  let patch = AudioClip.diff old_clip new_clip in
  match patch.AudioClip.Patch.fade with
  | `Modified fade_patch ->
    check bool "fade modified" true (not (Fade.Patch.is_empty fade_patch))
  | _ -> fail "Expected fade to be `Modified"

let test_diff_multiple_changes () =
  let old_clip = create_basic_clip () in
  let new_clip = { old_clip with name = "New Name"; start_time = 1.0; } in
  let patch = AudioClip.diff old_clip new_clip in
  check bool "name changed" true (match patch.AudioClip.Patch.name with `Modified _ -> true | _ -> false);
  check bool "start_time changed" true (match patch.AudioClip.Patch.start_time with `Modified _ -> true | _ -> false)

let test_diff_no_change () =
  let clip = create_basic_clip () in
  let patch = AudioClip.diff clip clip in
  check bool "patch empty" true (AudioClip.Patch.is_empty patch)

(* ID function tests *)
let test_has_same_id_same () =
  let clip = create_basic_clip () in
  check bool "same clip" true (AudioClip.has_same_id clip clip)

let test_has_same_id_different () =
  let clip1 = { (create_basic_clip ()) with AudioClip.id = 1 } in
  let clip2 = { (create_basic_clip ()) with AudioClip.id = 2 } in
  check bool "different clips" false (AudioClip.has_same_id clip1 clip2)

let test_id_hash_consistent () =
  let clip = { (create_basic_clip ()) with AudioClip.id = 42 } in
  let hash1 = AudioClip.id_hash clip in
  let hash2 = AudioClip.id_hash clip in
  check bool "same id same hash" true (hash1 = hash2)

(* Patch.is_empty tests *)
let test_patch_empty_no_changes () =
  let clip = create_basic_clip () in
  let patch = AudioClip.diff clip clip in
  check bool "patch empty" true (AudioClip.Patch.is_empty patch)

let test_patch_not_empty_with_changes () =
  let old_clip = create_basic_clip () in
  let new_clip = { old_clip with name = "Different" } in
  let patch = AudioClip.diff old_clip new_clip in
  check bool "patch not empty" false (AudioClip.Patch.is_empty patch)

let () =
  run "AudioClip" [
    "parsing", [
      test_case "parse AudioClip XML" `Quick test_create_audio_clip;
      test_case "fade enabled" `Quick test_fade_enabled;
      test_case "fade all fields" `Quick test_fade_all_fields;
      test_case "sample ref fields" `Quick test_sample_ref_fields;
      test_case "signature fields" `Quick test_signature_fields;
      test_case "loop fields" `Quick test_loop_fields;
    ];
    "diffing", [
      test_case "detect name changes" `Quick test_diff_name_change;
      test_case "detect start_time changes" `Quick test_diff_start_time_change;
      test_case "detect end_time changes" `Quick test_diff_end_time_change;
      test_case "detect loop changes" `Quick test_diff_loop_change;
      test_case "detect signature changes" `Quick test_diff_signature_change;
      test_case "detect sample_ref changes" `Quick test_diff_sample_ref_change;
      test_case "fade none to some" `Quick test_diff_fade_none_to_some;
      test_case "fade some to none" `Quick test_diff_fade_some_to_none;
      test_case "fade some to some" `Quick test_diff_fade_some_to_some;
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
