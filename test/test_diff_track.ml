
open Alsdiff_base
open Alsdiff_live
open Alsdiff_live.Track
open Track_helpers

(* Helper to create a dummy Midi Track *)
let make_midi_track id name mixer =
  Midi {
    MidiTrack.id = id;
    name = name;
    current_name = name;
    clips = [];
    automations = [];
    devices = [];
    mixer = mixer;
    routings = make_empty_routing_set ();
  }

let test_midi_track_diff () =
  let mixer1 = make_mixer 0.8 0.0 in
  let old_track = make_midi_track 1 "Midi Track" mixer1 in

  let mixer2 = make_mixer 0.5 0.0 in
  let new_track = make_midi_track 1 "Midi Track Renamed" mixer2 in

  let patch = Track.diff old_track new_track in

  (* Check that we got a MidiPatch *)
  (match patch with
   | Track.Patch.MidiPatch midi_patch ->
     (* Check name change *)
     (match midi_patch.Track.MidiTrack.Patch.name with
      | `Modified m ->
        Alcotest.(check string) "old name" "Midi Track" m.oldval;
        Alcotest.(check string) "new name" "Midi Track Renamed" m.newval
      | _ -> Alcotest.fail "Expected name to be modified");

     (* Check mixer volume change *)
     (match midi_patch.Track.MidiTrack.Patch.mixer with
      | `Modified mixer_patch ->
        (match mixer_patch.Track.Mixer.Patch.volume with
         | `Modified volume_patch ->
           (match volume_patch.Device.GenericParam.Patch.value with
            | `Modified v ->
              (match v.Diff.oldval with
               | Device.Float old_val -> Alcotest.(check (float 0.01)) "old volume" 0.8 old_val
               | _ -> Alcotest.fail "Expected Float value");
              (match v.Diff.newval with
               | Device.Float new_val -> Alcotest.(check (float 0.01)) "new volume" 0.5 new_val
               | _ -> Alcotest.fail "Expected Float value")
            | _ -> Alcotest.fail "Expected volume value to be modified")
         | _ -> Alcotest.fail "Expected volume to be modified")
      | _ -> Alcotest.fail "Expected mixer to be modified")
   | _ -> Alcotest.fail "Expected MidiPatch")

let test_audio_track_diff () =
  let mixer1 = make_mixer 0.7 0.0 in
  let old_track = Audio {
      AudioTrack.id = 10;
      name = "Audio Track";
      current_name = "Audio Track";
      clips = [];
      automations = [];
      devices = [];
      mixer = mixer1;
      routings = make_empty_routing_set ();
    } in

  let mixer2 = make_mixer 0.9 0.0 in
  let new_track = Audio {
      AudioTrack.id = 10;
      name = "Audio Track";
      current_name = "Audio Track";
      clips = [];
      automations = [];
      devices = [];
      mixer = mixer2;
      routings = make_empty_routing_set ();
    } in

  let patch = Track.diff old_track new_track in

  (* Check that we got an AudioPatch *)
  (match patch with
   | Track.Patch.AudioPatch audio_patch ->
     (* Check mixer volume change *)
     (match audio_patch.Track.AudioTrack.Patch.mixer with
      | `Modified mixer_patch ->
        (match mixer_patch.Track.Mixer.Patch.volume with
         | `Modified volume_patch ->
           (match volume_patch.Device.GenericParam.Patch.value with
            | `Modified v ->
              (match v.Diff.oldval with
               | Device.Float old_val -> Alcotest.(check (float 0.01)) "old volume" 0.7 old_val
               | _ -> Alcotest.fail "Expected Float value");
              (match v.Diff.newval with
               | Device.Float new_val -> Alcotest.(check (float 0.01)) "new volume" 0.9 new_val
               | _ -> Alcotest.fail "Expected Float value")
            | _ -> Alcotest.fail "Expected volume value to be modified")
         | _ -> Alcotest.fail "Expected volume to be modified")
      | _ -> Alcotest.fail "Expected mixer to be modified")
   | _ -> Alcotest.fail "Expected AudioPatch")


let test_group_track_diff () =
  let mixer1 = make_mixer 1.0 0.0 in
  let old_track = Group {
      AudioTrack.id = 91;
      name = "Bass";
      current_name = "Bass";
      clips = [];
      automations = [];
      devices = [];
      mixer = mixer1;
      routings = make_empty_routing_set ();
    } in

  let mixer2 = make_mixer 0.8 0.0 in
  let new_track = Group {
      AudioTrack.id = 91;
      name = "Bass";
      current_name = "Bass";
      clips = [];
      automations = [];
      devices = [];
      mixer = mixer2;
      routings = make_empty_routing_set ();
    } in

  let patch = Track.diff old_track new_track in

  (* Check that we got an AudioPatch (Group tracks use AudioTrack internally) *)
  (match patch with
   | Track.Patch.AudioPatch audio_patch ->
     (* Check mixer volume change *)
     (match audio_patch.Track.AudioTrack.Patch.mixer with
      | `Modified mixer_patch ->
        (match mixer_patch.Track.Mixer.Patch.volume with
         | `Modified volume_patch ->
           (match volume_patch.Device.GenericParam.Patch.value with
            | `Modified v ->
              (match v.Diff.oldval with
               | Device.Float old_val -> Alcotest.(check (float 0.01)) "old volume" 1.0 old_val
               | _ -> Alcotest.fail "Expected Float value");
              (match v.Diff.newval with
               | Device.Float new_val -> Alcotest.(check (float 0.01)) "new volume" 0.8 new_val
               | _ -> Alcotest.fail "Expected Float value")
            | _ -> Alcotest.fail "Expected volume value to be modified")
         | _ -> Alcotest.fail "Expected volume to be modified")
      | _ -> Alcotest.fail "Expected mixer to be modified")
   | _ -> Alcotest.fail "Expected AudioPatch")


let test_return_track_diff () =
  let mixer1 = make_mixer 1.0 (-0.5) in
  let old_track = Return {
      AudioTrack.id = 80;
      name = "Reverb";
      current_name = "Reverb";
      clips = [];
      automations = [];
      devices = [];
      mixer = mixer1;
      routings = make_empty_routing_set ();
    } in

  let mixer2 = make_mixer 1.0 0.5 in
  let new_track = Return {
      AudioTrack.id = 80;
      name = "Reverb";
      current_name = "Reverb";
      clips = [];
      automations = [];
      devices = [];
      mixer = mixer2;
      routings = make_empty_routing_set ();
    } in

  let patch = Track.diff old_track new_track in

  (* Check that we got an AudioPatch (Return tracks use AudioTrack internally) *)
  (match patch with
   | Track.Patch.AudioPatch audio_patch ->
     (* Check mixer pan change *)
     (match audio_patch.Track.AudioTrack.Patch.mixer with
      | `Modified mixer_patch ->
        (match mixer_patch.Track.Mixer.Patch.pan with
         | `Modified pan_patch ->
           (match pan_patch.Device.GenericParam.Patch.value with
            | `Modified v ->
              (match v.Diff.oldval with
               | Device.Float old_val -> Alcotest.(check (float 0.01)) "old pan" (-0.5) old_val
               | _ -> Alcotest.fail "Expected Float value");
              (match v.Diff.newval with
               | Device.Float new_val -> Alcotest.(check (float 0.01)) "new pan" 0.5 new_val
               | _ -> Alcotest.fail "Expected Float value")
            | _ -> Alcotest.fail "Expected pan value to be modified")
         | _ -> Alcotest.fail "Expected pan to be modified")
      | _ -> Alcotest.fail "Expected mixer to be modified")
   | _ -> Alcotest.fail "Expected AudioPatch")


let test_main_track_diff () =
  let make_main_mixer tempo =
    let base = make_mixer 1.0 0.0 in
    {
      Track.MainMixer.base;
      tempo = make_generic_param "Tempo" (Device.Float tempo);
      time_signature = make_generic_param "TimeSignature" (Device.Int 4);
      crossfade = make_generic_param "CrossFade" (Device.Float 1.0);
      global_groove = make_generic_param "GlobalGroove" (Device.Float 0.0);
    }
  in

  let old_track = Main {
      MainTrack.name = "Master";
      current_name = "Master";
      automations = [];
      devices = [];
      mixer = make_main_mixer 120.0;
      routings = make_empty_routing_set ();
    } in

  let new_track = Main {
      MainTrack.name = "Master";
      current_name = "Master";
      automations = [];
      devices = [];
      mixer = make_main_mixer 128.0;
      routings = make_empty_routing_set ();
    } in

  let patch = Track.diff old_track new_track in

  (* Check that we got a MainPatch *)
  (match patch with
   | Track.Patch.MainPatch main_patch ->
     (* Check mixer tempo change *)
     (match main_patch.Track.MainTrack.Patch.mixer with
      | `Modified mixer_patch ->
        (match mixer_patch.Track.MainMixer.Patch.tempo with
         | `Modified tempo_patch ->
           (match tempo_patch.Device.GenericParam.Patch.value with
            | `Modified v ->
              (match v.Diff.oldval with
               | Device.Float old_val -> Alcotest.(check (float 0.01)) "old tempo" 120.0 old_val
               | _ -> Alcotest.fail "Expected Float value");
              (match v.Diff.newval with
               | Device.Float new_val -> Alcotest.(check (float 0.01)) "new tempo" 128.0 new_val
               | _ -> Alcotest.fail "Expected Float value")
            | _ -> Alcotest.fail "Expected tempo value to be modified")
         | _ -> Alcotest.fail "Expected tempo to be modified")
      | _ -> Alcotest.fail "Expected mixer to be modified")
   | _ -> Alcotest.fail "Expected MainPatch")


let test_cross_type_diff_error () =
  let mixer = make_mixer 0.8 0.0 in
  let midi_track = Midi {
      MidiTrack.id = 1;
      name = "Midi Track";
      current_name = "Midi Track";
      clips = []; automations = []; devices = [];
      mixer; routings = make_empty_routing_set ();
    } in

  let audio_track = Audio {
      AudioTrack.id = 1;
      name = "Audio Track";
      current_name = "Audio Track";
      clips = []; automations = []; devices = [];
      mixer; routings = make_empty_routing_set ();
    } in

  (* Cross-type diff should raise an exception *)
  let exception_thrown = try
      let _ = Track.diff midi_track audio_track in
      false
    with
    | Failure _ -> true
    | _ -> false
  in

  Alcotest.(check bool) "cross-type diff raises exception" true exception_thrown


let test_same_track_no_changes () =
  let mixer = make_mixer 0.8 0.0 in
  let track = Midi {
      MidiTrack.id = 1;
      name = "Midi Track";
      current_name = "Midi Track";
      clips = []; automations = []; devices = [];
      mixer; routings = make_empty_routing_set ();
    } in

  let patch = Track.diff track track in

  (* Same track should produce empty patch *)
  (match patch with
   | Track.Patch.MidiPatch midi_patch ->
     Alcotest.(check bool) "same track produces empty patch"
       true (MidiTrack.Patch.is_empty midi_patch)
   | _ -> Alcotest.fail "Expected MidiPatch")


let () =
  Alcotest.run "Diff Track" [
    "midi-track", [
      Alcotest.test_case "Test midi track diff" `Quick test_midi_track_diff;
      Alcotest.test_case "Test same track no changes" `Quick test_same_track_no_changes;
    ];
    "audio-track", [
      Alcotest.test_case "Test audio track diff" `Quick test_audio_track_diff;
    ];
    "group-track", [
      Alcotest.test_case "Test group track diff" `Quick test_group_track_diff;
    ];
    "return-track", [
      Alcotest.test_case "Test return track diff" `Quick test_return_track_diff;
    ];
    "main-track", [
      Alcotest.test_case "Test main track diff" `Quick test_main_track_diff;
    ];
    "cross-type", [
      Alcotest.test_case "Test cross-type diff error" `Quick test_cross_type_diff_error;
    ];
  ]
