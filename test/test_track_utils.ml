open Alsdiff_live
open Alsdiff_live.Track
open Track_helpers

let test_get_name_midi_track () =
  let mixer = make_mixer 0.8 0.0 in
  let track = Midi {
      MidiTrack.id = 1;
      name = "Midi Track Name";
      current_name = "Midi Track Name";
      group_id = -1;
      clips = [];
      automations = [];
      devices = [];
      mixer;
      routings = make_empty_routing_set ();
    } in
  Alcotest.(check string) "get_name MidiTrack" "Midi Track Name" (Track.get_name track)


let test_get_name_audio_track () =
  let mixer = make_mixer 0.8 0.0 in
  let track = Audio {
      AudioTrack.id = 2;
      name = "Audio Track Name";
      current_name = "Audio Track Name";
      group_id = -1;
      clips = [];
      automations = [];
      devices = [];
      mixer;
      routings = make_empty_routing_set ();
    } in
  Alcotest.(check string) "get_name AudioTrack" "Audio Track Name" (Track.get_name track)


let test_get_name_group_track () =
  let mixer = make_mixer 0.8 0.0 in
  let track = Group {
      AudioTrack.id = 3;
      name = "Group Track Name";
      current_name = "Group Track Name";
      group_id = -1;
      clips = [];
      automations = [];
      devices = [];
      mixer;
      routings = make_empty_routing_set ();
    } in
  Alcotest.(check string) "get_name GroupTrack" "Group Track Name" (Track.get_name track)


let test_get_name_return_track () =
  let mixer = make_mixer 0.8 0.0 in
  let track = Return {
      AudioTrack.id = 4;
      name = "Return Track Name";
      current_name = "Return Track Name";
      group_id = -1;
      clips = [];
      automations = [];
      devices = [];
      mixer;
      routings = make_empty_routing_set ();
    } in
  Alcotest.(check string) "get_name ReturnTrack" "Return Track Name" (Track.get_name track)


let test_get_name_main_track () =
  let mixer = make_main_mixer () in
  let track = Main {
      MainTrack.name = "Master";
      current_name = "Master";
      automations = [];
      devices = [];
      mixer;
      routings = make_empty_routing_set ();
    } in
  Alcotest.(check string) "get_name MainTrack" "Main" (Track.get_name track)


let test_type_name_all_track_types () =
  let mixer = make_mixer 0.8 0.0 in
  let main_mixer = make_main_mixer () in

  let midi_track = Midi {
      MidiTrack.id = 1;
      name = "Midi";
      current_name = "Midi";
      group_id = -1;
      clips = []; automations = []; devices = [];
      mixer; routings = make_empty_routing_set ();
    } in

  let audio_track = Audio {
      AudioTrack.id = 2;
      name = "Audio";
      current_name = "Audio";
      group_id = -1;
      clips = []; automations = []; devices = [];
      mixer; routings = make_empty_routing_set ();
    } in

  let group_track = Group {
      AudioTrack.id = 3;
      name = "Group";
      current_name = "Group";
      group_id = -1;
      clips = []; automations = []; devices = [];
      mixer; routings = make_empty_routing_set ();
    } in

  let return_track = Return {
      AudioTrack.id = 4;
      name = "Return";
      current_name = "Return";
      group_id = -1;
      clips = []; automations = []; devices = [];
      mixer; routings = make_empty_routing_set ();
    } in

  let main_track = Main {
      MainTrack.name = "Master";
      current_name = "Master";
      automations = []; devices = [];
      mixer = main_mixer; routings = make_empty_routing_set ();
    } in

  Alcotest.(check string) "type_name Midi" "MidiTrack" (Track.type_name midi_track);
  Alcotest.(check string) "type_name Audio" "AudioTrack" (Track.type_name audio_track);
  Alcotest.(check string) "type_name Group" "GroupTrack" (Track.type_name group_track);
  Alcotest.(check string) "type_name Return" "ReturnTrack" (Track.type_name return_track);
  Alcotest.(check string) "type_name Main" "MainTrack" (Track.type_name main_track)


let test_has_same_id_midi_track () =
  let mixer = make_mixer 0.8 0.0 in
  let track1 = Midi {
      MidiTrack.id = 1;
      name = "Track 1";
      current_name = "Track 1";
      group_id = -1;
      clips = []; automations = []; devices = [];
      mixer; routings = make_empty_routing_set ();
    } in
  let track2 = Midi {
      MidiTrack.id = 1;
      name = "Track 2";
      current_name = "Track 2";
      group_id = -1;
      clips = []; automations = []; devices = [];
      mixer; routings = make_empty_routing_set ();
    } in
  let track3 = Midi {
      MidiTrack.id = 2;
      name = "Track 3";
      current_name = "Track 3";
      group_id = -1;
      clips = []; automations = []; devices = [];
      mixer; routings = make_empty_routing_set ();
    } in

  Alcotest.(check bool) "has_same_id same id" true (Track.has_same_id track1 track2);
  Alcotest.(check bool) "has_same_id different id" false (Track.has_same_id track1 track3)


let test_has_same_id_audio_track () =
  let mixer = make_mixer 0.8 0.0 in
  let track1 = Audio {
      AudioTrack.id = 10;
      name = "Audio 1";
      current_name = "Audio 1";
      group_id = -1;
      clips = []; automations = []; devices = [];
      mixer; routings = make_empty_routing_set ();
    } in
  let track2 = Audio {
      AudioTrack.id = 10;
      name = "Audio 2";
      current_name = "Audio 2";
      group_id = -1;
      clips = []; automations = []; devices = [];
      mixer; routings = make_empty_routing_set ();
    } in

  Alcotest.(check bool) "has_same_id Audio same id" true (Track.has_same_id track1 track2)


let test_has_same_id_group_track () =
  let mixer = make_mixer 0.8 0.0 in
  let track1 = Group {
      AudioTrack.id = 20;
      name = "Group 1";
      current_name = "Group 1";
      group_id = -1;
      clips = []; automations = []; devices = [];
      mixer; routings = make_empty_routing_set ();
    } in
  let track2 = Group {
      AudioTrack.id = 20;
      name = "Group 2";
      current_name = "Group 2";
      group_id = -1;
      clips = []; automations = []; devices = [];
      mixer; routings = make_empty_routing_set ();
    } in

  Alcotest.(check bool) "has_same_id Group same id" true (Track.has_same_id track1 track2)


let test_has_same_id_return_track () =
  let mixer = make_mixer 0.8 0.0 in
  let track1 = Return {
      AudioTrack.id = 30;
      name = "Return 1";
      current_name = "Return 1";
      group_id = -1;
      clips = []; automations = []; devices = [];
      mixer; routings = make_empty_routing_set ();
    } in
  let track2 = Return {
      AudioTrack.id = 30;
      name = "Return 2";
      current_name = "Return 2";
      group_id = -1;
      clips = []; automations = []; devices = [];
      mixer; routings = make_empty_routing_set ();
    } in

  Alcotest.(check bool) "has_same_id Return same id" true (Track.has_same_id track1 track2)


let test_has_same_id_main_track_always_true () =
  let mixer = make_main_mixer () in
  let track1 = Main {
      MainTrack.name = "Master";
      current_name = "Master";
      automations = []; devices = [];
      mixer; routings = make_empty_routing_set ();
    } in
  let track2 = Main {
      MainTrack.name = "Master";
      current_name = "Master";
      automations = []; devices = [];
      mixer; routings = make_empty_routing_set ();
    } in

  (* MainTrack uses singleton pattern - has_same_id always returns true *)
  Alcotest.(check bool) "has_same_id Main always true" true (Track.has_same_id track1 track2)


let test_has_same_id_cross_type () =
  let mixer = make_mixer 0.8 0.0 in
  let midi_track = Midi {
      MidiTrack.id = 1;
      name = "Midi";
      current_name = "Midi";
      group_id = -1;
      clips = []; automations = []; devices = [];
      mixer; routings = make_empty_routing_set ();
    } in
  let audio_track = Audio {
      AudioTrack.id = 1;
      name = "Audio";
      current_name = "Audio";
      group_id = -1;
      clips = []; automations = []; devices = [];
      mixer; routings = make_empty_routing_set ();
    } in

  (* Cross-type comparisons always return false *)
  Alcotest.(check bool) "has_same_id cross-type false" false (Track.has_same_id midi_track audio_track)


let test_id_hash_midi_track () =
  let mixer = make_mixer 0.8 0.0 in
  let track1 = Midi {
      MidiTrack.id = 42;
      name = "Track";
      current_name = "Track";
      group_id = -1;
      clips = []; automations = []; devices = [];
      mixer; routings = make_empty_routing_set ();
    } in
  let track2 = Midi {
      MidiTrack.id = 42;
      name = "Other Track";
      current_name = "Other Track";
      group_id = -1;
      clips = []; automations = []; devices = [];
      mixer; routings = make_empty_routing_set ();
    } in

  (* Same ID should produce same hash *)
  let hash1 = Track.id_hash track1 in
  let hash2 = Track.id_hash track2 in
  Alcotest.(check int) "id_hash same id same hash" hash1 hash2


let test_id_hash_main_track_constant () =
  let mixer = make_main_mixer () in
  let track1 = Main {
      MainTrack.name = "Master";
      current_name = "Master";
      automations = []; devices = [];
      mixer; routings = make_empty_routing_set ();
    } in
  let track2 = Main {
      MainTrack.name = "Master";
      current_name = "Master";
      automations = []; devices = [];
      mixer; routings = make_empty_routing_set ();
    } in

  (* MainTrack uses constant hash (Hashtbl.hash 0) *)
  let expected_hash = Hashtbl.hash 0 in
  Alcotest.(check int) "id_hash Main constant" expected_hash (Track.id_hash track1);
  Alcotest.(check int) "id_hash Main consistent" expected_hash (Track.id_hash track2)


let test_patch_is_empty_midi_patch () =
  (* Create an empty patch by diffing identical tracks *)
  let mixer = make_mixer 0.8 0.0 in
  let track = Midi {
      MidiTrack.id = 1;
      name = "Test";
      current_name = "Test";
      group_id = -1;
      clips = []; automations = []; devices = [];
      mixer; routings = make_empty_routing_set ();
    } in
  let patch = Track.diff track track in

  Alcotest.(check bool) "is_empty MidiPatch" true
    (Track.Patch.is_empty patch)


let test_patch_is_empty_audio_patch () =
  (* Create an empty patch by diffing identical tracks *)
  let mixer = make_mixer 0.8 0.0 in
  let track = Audio {
      AudioTrack.id = 2;
      name = "Test";
      current_name = "Test";
      group_id = -1;
      clips = []; automations = []; devices = [];
      mixer; routings = make_empty_routing_set ();
    } in
  let patch = Track.diff track track in

  Alcotest.(check bool) "is_empty AudioPatch" true
    (Track.Patch.is_empty patch)


let test_patch_is_empty_main_patch () =
  (* Create an empty patch by diffing identical tracks *)
  let main_mixer = make_main_mixer () in
  let track = Main {
      MainTrack.name = "Master";
      current_name = "Master";
      automations = []; devices = [];
      mixer = main_mixer; routings = make_empty_routing_set ();
    } in
  let patch = Track.diff track track in

  Alcotest.(check bool) "is_empty MainPatch" true
    (Track.Patch.is_empty patch)


let test_patch_is_not_empty () =
  (* Create a non-empty patch by diffing different tracks *)
  let mixer1 = make_mixer 0.8 0.0 in
  let mixer2 = make_mixer 0.9 0.0 in
  let track1 = Midi {
      MidiTrack.id = 1;
      name = "Old Name";
      current_name = "Old Name";
      group_id = -1;
      clips = []; automations = []; devices = [];
      mixer = mixer1; routings = make_empty_routing_set ();
    } in
  let track2 = Midi {
      MidiTrack.id = 1;
      name = "New Name";
      current_name = "New Name";
      group_id = -1;
      clips = []; automations = []; devices = [];
      mixer = mixer2; routings = make_empty_routing_set ();
    } in
  let patch = Track.diff track1 track2 in

  Alcotest.(check bool) "is_not_empty with modification" false
    (Track.Patch.is_empty patch)


let () =
  Alcotest.run "Track Utils" [
    "get_name", [
      Alcotest.test_case "get_name MidiTrack" `Quick test_get_name_midi_track;
      Alcotest.test_case "get_name AudioTrack" `Quick test_get_name_audio_track;
      Alcotest.test_case "get_name GroupTrack" `Quick test_get_name_group_track;
      Alcotest.test_case "get_name ReturnTrack" `Quick test_get_name_return_track;
      Alcotest.test_case "get_name MainTrack" `Quick test_get_name_main_track;
    ];
    "type_name", [
      Alcotest.test_case "type_name all track types" `Quick test_type_name_all_track_types;
    ];
    "has_same_id", [
      Alcotest.test_case "has_same_id MidiTrack" `Quick test_has_same_id_midi_track;
      Alcotest.test_case "has_same_id AudioTrack" `Quick test_has_same_id_audio_track;
      Alcotest.test_case "has_same_id GroupTrack" `Quick test_has_same_id_group_track;
      Alcotest.test_case "has_same_id ReturnTrack" `Quick test_has_same_id_return_track;
      Alcotest.test_case "has_same_id MainTrack always true" `Quick test_has_same_id_main_track_always_true;
      Alcotest.test_case "has_same_id cross-type false" `Quick test_has_same_id_cross_type;
    ];
    "id_hash", [
      Alcotest.test_case "id_hash MidiTrack consistent" `Quick test_id_hash_midi_track;
      Alcotest.test_case "id_hash MainTrack constant" `Quick test_id_hash_main_track_constant;
    ];
    "Patch.is_empty", [
      Alcotest.test_case "is_empty MidiPatch" `Quick test_patch_is_empty_midi_patch;
      Alcotest.test_case "is_empty AudioPatch" `Quick test_patch_is_empty_audio_patch;
      Alcotest.test_case "is_empty MainPatch" `Quick test_patch_is_empty_main_patch;
      Alcotest.test_case "is_not_empty with modification" `Quick test_patch_is_not_empty;
    ];
  ]
