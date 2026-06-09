open Alsdiff_base.Xml
open Alsdiff_live.Track
open Alsdiff_live
open Utils

let test_group_track_xml_path = resolve_test_data_path "group_track.xml"


let test_group_track_basic_properties () =
  (* Load group track XML file *)
  let xml = read_file test_group_track_xml_path in

  (* Create track from XML - should return Group variant *)
  let track = Track.create xml in

  (* Verify it's a GroupTrack *)
  (match track with
   | Track.Group group_track ->
     (* Expected values based on XML file *)
     let expected_id = 91 in
     let expected_name = "Bass" in

     (* Test basic fields *)
     Alcotest.(check int) "id" expected_id group_track.AudioTrack.id;
     Alcotest.(check string) "name" expected_name group_track.name
   | _ -> Alcotest.fail "Expected Group track variant")


let test_group_track_mixer_properties () =
  (* Load group track XML file *)
  let xml = read_file test_group_track_xml_path in

  (* Create track from XML *)
  let track = Track.create xml in

  (* Verify it's a GroupTrack *)
  (match track with
   | Track.Group group_track ->
     (* Check mixer properties *)
     let volume = match group_track.mixer.volume.value with
       | Device.Float f -> f
       | _ -> 0.0
     in
     let pan = match group_track.mixer.pan.value with
       | Device.Float f -> f
       | _ -> 0.0
     in
     let mute = match group_track.mixer.mute.value with
       | Device.Bool b -> b
       | _ -> false
     in
     let solo = match group_track.mixer.solo.value with
       | Device.Bool b -> b
       | _ -> false
     in

     Alcotest.(check (float 0.001)) "volume" 1.0 volume;
     Alcotest.(check (float 0.001)) "pan" 0.0 pan;
     Alcotest.(check bool) "mute" true mute;
     Alcotest.(check bool) "solo" false solo;

     (* Check sends - should have one send *)
     Alcotest.(check int) "send count" 1 (List.length group_track.mixer.sends)
   | _ -> Alcotest.fail "Expected Group track variant")


let test_group_track_device_parsing () =
  (* Load group track XML file *)
  let xml = read_file test_group_track_xml_path in

  (* Create track from XML *)
  let track = Track.create xml in

  (* Verify it's a GroupTrack *)
  (match track with
   | Track.Group group_track ->
     (* Check that we have devices *)
     let device_count = List.length group_track.devices in
     Alcotest.(check int) "device count" 1 device_count;

     (* Check device name - device_name and display_name are both "Eq8" *)
     if device_count > 0 then
       let first_device = List.hd group_track.devices in
       (match first_device with
        | Device.Regular reg ->
          Alcotest.(check string) "device name" "Eq8" reg.device_name;
          Alcotest.(check string) "display name" "Eq8" reg.display_name
        | _ -> Alcotest.fail "Expected Regular device")
     else
       Alcotest.fail "Expected at least one device in group track"
   | _ -> Alcotest.fail "Expected Group track variant")


let test_group_track_routing () =
  (* Load group track XML file *)
  let xml = read_file test_group_track_xml_path in

  (* Create track from XML *)
  let track = Track.create xml in

  (* Verify it's a GroupTrack *)
  (match track with
   | Track.Group group_track ->
     (* Check audio output routing - should be "Master" *)
     Alcotest.(check string) "audio out target" "AudioOut/Main"
       group_track.routings.RoutingSet.audio_out.target;
     Alcotest.(check string) "audio out upper" "Master"
       group_track.routings.audio_out.upper_string;

     (* Check audio input routing *)
     Alcotest.(check string) "audio in target" "AudioIn/External/S0"
       group_track.routings.audio_in.target;
     Alcotest.(check string) "audio in upper" "Ext. In"
       group_track.routings.audio_in.upper_string
   | _ -> Alcotest.fail "Expected Group track variant")


let test_group_track_clips_empty () =
  (* Load group track XML file *)
  let xml = read_file test_group_track_xml_path in

  (* Create track from XML *)
  let track = Track.create xml in

  (* Verify it's a GroupTrack *)
  (match track with
   | Track.Group group_track ->
     (* Group tracks shouldn't have audio clips *)
     Alcotest.(check int) "clip count" 0 (List.length group_track.arrangement_clips)
   | _ -> Alcotest.fail "Expected Group track variant")


let () =
  Alcotest.run "GroupTrack" [
    "track_creation", [
      Alcotest.test_case "parse basic GroupTrack properties" `Quick test_group_track_basic_properties;
      Alcotest.test_case "parse GroupTrack mixer properties" `Quick test_group_track_mixer_properties;
      Alcotest.test_case "parse GroupTrack device (EQ Eight)" `Quick test_group_track_device_parsing;
      Alcotest.test_case "parse GroupTrack routing configuration" `Quick test_group_track_routing;
      Alcotest.test_case "parse GroupTrack clips (should be empty)" `Quick test_group_track_clips_empty;
    ]
  ]
