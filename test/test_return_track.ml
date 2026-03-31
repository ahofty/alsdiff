open Alsdiff_base.Xml
open Alsdiff_live.Track
open Alsdiff_live
open Utils

let test_return_track_xml_path = resolve_test_data_path "return_track.xml"


let test_return_track_basic_properties () =
  (* Load return track XML file *)
  let xml = read_file test_return_track_xml_path in

  (* Create track from XML - should return Return variant *)
  let track = Track.create xml in

  (* Verify it's a ReturnTrack *)
  (match track with
   | Track.Return return_track ->
     (* Expected values based on XML file *)
     let expected_id = 80 in
     let expected_name = "A-Room Reverb" in

     (* Test basic fields *)
     Alcotest.(check int) "id" expected_id return_track.AudioTrack.id;
     Alcotest.(check string) "name" expected_name return_track.name
   | _ -> Alcotest.fail "Expected Return track variant")


let test_return_track_mixer_properties () =
  (* Load return track XML file *)
  let xml = read_file test_return_track_xml_path in

  (* Create track from XML *)
  let track = Track.create xml in

  (* Verify it's a ReturnTrack *)
  (match track with
   | Track.Return return_track ->
     (* Check mixer properties *)
     let volume = match return_track.mixer.volume.value with
       | Device.Float f -> f
       | _ -> 0.0
     in
     let pan = match return_track.mixer.pan.value with
       | Device.Float f -> f
       | _ -> 0.0
     in
     let mute = match return_track.mixer.mute.value with
       | Device.Bool b -> b
       | _ -> false
     in
     let solo = match return_track.mixer.solo.value with
       | Device.Bool b -> b
       | _ -> false
     in

     Alcotest.(check (float 0.001)) "volume" 1.0 volume;
     Alcotest.(check (float 0.001)) "pan" 0.0 pan;
     Alcotest.(check bool) "mute" true mute;
     Alcotest.(check bool) "solo" false solo;

     (* Check sends - should have one send *)
     Alcotest.(check int) "send count" 1 (List.length return_track.mixer.sends)
   | _ -> Alcotest.fail "Expected Return track variant")


let test_return_track_device_parsing () =
  (* Load return track XML file *)
  let xml = read_file test_return_track_xml_path in

  (* Create track from XML *)
  let track = Track.create xml in

  (* Verify it's a ReturnTrack *)
  (match track with
   | Track.Return return_track ->
     (* Check that we have devices *)
     let device_count = List.length return_track.devices in
     Alcotest.(check int) "device count" 1 device_count;

     (* Check device name - both device_name and display_name are "Hybrid" *)
     if device_count > 0 then
       let first_device = List.hd return_track.devices in
       (match first_device with
        | Device.Regular reg ->
          Alcotest.(check string) "device name" "Hybrid" reg.device_name;
          Alcotest.(check string) "display name" "Hybrid" reg.display_name
        | _ -> Alcotest.fail "Expected Regular device")
     else
       Alcotest.fail "Expected at least one device in return track"
   | _ -> Alcotest.fail "Expected Return track variant")


let test_return_track_routing () =
  (* Load return track XML file *)
  let xml = read_file test_return_track_xml_path in

  (* Create track from XML *)
  let track = Track.create xml in

  (* Verify it's a ReturnTrack *)
  (match track with
   | Track.Return return_track ->
     (* Check audio output routing - should be "Master" *)
     Alcotest.(check string) "audio out target" "AudioOut/Main"
       return_track.routings.RoutingSet.audio_out.target;
     Alcotest.(check string) "audio out upper" "Master"
       return_track.routings.audio_out.upper_string;

     (* Check audio input routing - should be "AudioIn/External" *)
     Alcotest.(check string) "audio in target" "AudioIn/External/S0"
       return_track.routings.audio_in.target;
     Alcotest.(check string) "audio in upper" "Ext. In"
       return_track.routings.audio_in.upper_string
   | _ -> Alcotest.fail "Expected Return track variant")


let test_return_track_clips_empty () =
  (* Load return track XML file *)
  let xml = read_file test_return_track_xml_path in

  (* Create track from XML *)
  let track = Track.create xml in

  (* Verify it's a ReturnTrack *)
  (match track with
   | Track.Return return_track ->
     (* Return tracks shouldn't have audio clips *)
     Alcotest.(check int) "clip count" 0 (List.length return_track.clips)
   | _ -> Alcotest.fail "Expected Return track variant")


let () =
  Alcotest.run "ReturnTrack" [
    "track_creation", [
      Alcotest.test_case "parse basic ReturnTrack properties" `Quick test_return_track_basic_properties;
      Alcotest.test_case "parse ReturnTrack mixer properties" `Quick test_return_track_mixer_properties;
      Alcotest.test_case "parse ReturnTrack device (Hybrid Reverb)" `Quick test_return_track_device_parsing;
      Alcotest.test_case "parse ReturnTrack routing configuration" `Quick test_return_track_routing;
      Alcotest.test_case "parse ReturnTrack clips (should be empty)" `Quick test_return_track_clips_empty;
    ]
  ]
