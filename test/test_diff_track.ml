
open Alsdiff_base
open Alsdiff_live
open Alsdiff_live.Track



(* Helper to create a dummy Device.GenericParam.t *)
let make_generic_param name value =
  {
    Device.GenericParam.name = name;
    value = value;
    automation = 0;
    modulation = 0;
    mapping = None;
  }


(* Helper to create a dummy Track.Mixer *)
let make_mixer volume pan =
  {
    Track.Mixer.volume = make_generic_param "Volume" (Device.Float volume);
    Track.Mixer.pan = make_generic_param "Pan" (Device.Float pan);
    Track.Mixer.mute = make_generic_param "On" (Device.Bool false);
    Track.Mixer.solo = make_generic_param "Sololink" (Device.Bool false);
    Track.Mixer.sends = [];
  }

(* Helper to create a dummy RoutingSet.t *)
let make_empty_routing_set () =
  let make_routing route_type =
    {
      Track.Routing.route_type;
      target = "";
      upper_string = "";
      lower_string = "";
    }
  in
  {
    Track.RoutingSet.audio_in = make_routing Track.Routing.AudioIn;
    audio_out = make_routing Track.Routing.AudioOut;
    midi_in = make_routing Track.Routing.MidiIn;
    midi_out = make_routing Track.Routing.MidiOut;
  }

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

let make_main_track ?(name = "Main") tempo =
  let base_mixer = make_mixer 0.8 0.0 in
  Main {
    MainTrack.name = name;
    current_name = name;
    automations = [];
    devices = [];
    mixer = {
      Track.MainMixer.base = base_mixer;
      tempo = make_generic_param "Tempo" (Device.Float tempo);
      time_signature = make_generic_param "TimeSignature" (Device.Int 4);
      crossfade = make_generic_param "CrossFade" (Device.Float 0.0);
      global_groove = make_generic_param "GlobalGrooveAmount" (Device.Float 0.0);
    };
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

let test_main_track_diff () =
  let old_track = make_main_track 120.0 in
  let new_track = make_main_track 128.0 in
  let patch = Track.diff old_track new_track in

  match patch with
  | Track.Patch.MainPatch main_patch ->
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
     | _ -> Alcotest.fail "Expected main mixer to be modified")
  | _ -> Alcotest.fail "Expected MainPatch"

let () =
  Alcotest.run "Diff Track" [
    "midi-track", [
      Alcotest.test_case "Test midi track diff" `Quick test_midi_track_diff;
    ];
    "main-track", [
      Alcotest.test_case "Test main track diff" `Quick test_main_track_diff;
    ];
  ]
