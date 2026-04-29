open Alsdiff_base
open Alsdiff_base.Xml
open Alsdiff_live.Track
open Alsdiff_live

let test_main_track_xml_path = Utils.resolve_test_data_path "main_track.xml"

let test_main_track_basic_properties () =
  (* Load main track XML file *)
  let xml = read_file test_main_track_xml_path in

  (* Create main track from XML *)
  let main_track = MainTrack.create xml in

  (* Expected values based on XML file *)
  let expected_name = "Main" in

  (* Test basic fields *)
  Alcotest.(check string) "name" expected_name main_track.name

let test_main_track_automations () =
  (* Load main track XML file *)
  let xml = read_file test_main_track_xml_path in

  (* Create main track from XML *)
  let main_track = MainTrack.create xml in

  (* Check that we have the expected number of automation envelopes *)
  Alcotest.(check int) "automation count" 2 (List.length main_track.automations);

  (* Test first automation envelope details *)
  let first_automation = List.hd main_track.automations in
  Alcotest.(check int) "first automation id" 0 first_automation.Automation.id;

  (* Test second automation envelope details *)
  let second_automation = List.nth main_track.automations 1 in
  Alcotest.(check int) "second automation id" 1 second_automation.Automation.id;

  (* Test all automation IDs are present and in order *)
  let expected_automation_ids = [0; 1] in
  let actual_automation_ids = List.map (fun auto -> auto.Automation.id) main_track.automations in
  Alcotest.(check (list int)) "automation IDs order" expected_automation_ids actual_automation_ids

let test_main_track_devices () =
  (* Load main track XML file *)
  let xml = read_file test_main_track_xml_path in

  (* Create main track from XML *)
  let main_track = MainTrack.create xml in

  (* Check that devices are correctly parsed *)
  Alcotest.(check int) "device count" 1 (List.length main_track.devices);

  (* Test first device details *)
  let first_device = List.hd main_track.devices in
  (match first_device with
   | Device.Regular rd ->
     Alcotest.(check int) "first device id" 0 rd.id;
     Alcotest.(check string) "first device type" "Limiter" rd.device_name
   | _ -> Alcotest.fail "expected Regular device for Limiter")

let test_main_track_mixer () =
  (* Load main track XML file *)
  let xml = read_file test_main_track_xml_path in

  (* Create main track from XML *)
  let main_track = MainTrack.create xml in

  (* Test MainMixer-specific parameters *)
  let tempo_value = match main_track.mixer.tempo.Device.GenericParam.value with
    | Device.Float f -> f
    | _ -> 0.0
  in
  let time_signature_value = match main_track.mixer.time_signature.Device.GenericParam.value with
    | Device.Int i -> i
    | _ -> 0
  in
  let crossfade_value = match main_track.mixer.crossfade.Device.GenericParam.value with
    | Device.Int i -> i
    | _ -> 0
  in
  let global_groove_value = match main_track.mixer.global_groove.Device.GenericParam.value with
    | Device.Float f -> f
    | _ -> 0.0
  in

  (* Test MainMixer parameters against XML values *)
  Alcotest.(check (float 0.001)) "tempo value" 120.0 tempo_value;
  Alcotest.(check int) "time signature value" 201 time_signature_value;
  Alcotest.(check int) "crossfade value" 0 crossfade_value;
  Alcotest.(check (float 0.001)) "global groove value" 100.0 global_groove_value;

  (* Test base mixer properties *)
  let volume_value = match main_track.mixer.base.Track.Mixer.volume.Device.GenericParam.value with
    | Device.Float f -> f
    | _ -> 0.0
  in
  let pan_value = match main_track.mixer.base.Track.Mixer.pan.Device.GenericParam.value with
    | Device.Float f -> f
    | _ -> 0.0
  in
  let mute_value = match main_track.mixer.base.Track.Mixer.mute.Device.GenericParam.value with
    | Device.Bool b -> b
    | _ -> false
  in

  Alcotest.(check (float 0.001)) "volume value" 1.0 volume_value;
  Alcotest.(check (float 0.001)) "pan value" 0.0 pan_value;
  Alcotest.(check bool) "mute value" true mute_value

let test_main_track_routing () =
  (* Load main track XML file *)
  let xml = read_file test_main_track_xml_path in

  (* Create main track from XML *)
  let main_track = MainTrack.create xml in

  (* Test audio input routing *)
  Alcotest.(check string) "audio in target" "AudioIn/External/S0" main_track.routings.Track.RoutingSet.audio_in.target;
  Alcotest.(check string) "audio in upper display" "Ext. In" main_track.routings.Track.RoutingSet.audio_in.upper_string;
  Alcotest.(check string) "audio in lower display" "1/2" main_track.routings.Track.RoutingSet.audio_in.lower_string;

  (* Test audio output routing *)
  Alcotest.(check string) "audio out target" "AudioOut/External/S0" main_track.routings.Track.RoutingSet.audio_out.target;
  Alcotest.(check string) "audio out upper display" "Ext. Out" main_track.routings.Track.RoutingSet.audio_out.upper_string;
  Alcotest.(check string) "audio out lower display" "1/2" main_track.routings.Track.RoutingSet.audio_out.lower_string;

  (* Test MIDI input routing *)
  Alcotest.(check string) "midi in target" "MidiIn/External.All/-1" main_track.routings.Track.RoutingSet.midi_in.target;
  Alcotest.(check string) "midi in upper display" "Ext: All Ins" main_track.routings.Track.RoutingSet.midi_in.upper_string;
  Alcotest.(check string) "midi in lower display" "" main_track.routings.Track.RoutingSet.midi_in.lower_string;

  (* Test MIDI output routing *)
  Alcotest.(check string) "midi out target" "MidiOut/None" main_track.routings.Track.RoutingSet.midi_out.target;
  Alcotest.(check string) "midi out upper display" "None" main_track.routings.Track.RoutingSet.midi_out.upper_string;
  Alcotest.(check string) "midi out lower display" "" main_track.routings.Track.RoutingSet.midi_out.lower_string

let test_main_track_comprehensive () =
  (* Load main track XML file *)
  let xml = read_file test_main_track_xml_path in

  (* Create main track from XML *)
  let main_track = MainTrack.create xml in

  (* Comprehensive test of all fields together *)
  Alcotest.(check string) "name" "Main" main_track.name;
  Alcotest.(check int) "automation count" 2 (List.length main_track.automations);
  Alcotest.(check int) "device count" 1 (List.length main_track.devices);

  (* Verify first automation ID and first device ID *)
  let first_automation_id = (List.hd main_track.automations).Automation.id in
  let first_device_id = (match List.hd main_track.devices with
      | Device.Regular rd -> rd.id
      | _ -> -1) in
  Alcotest.(check int) "first automation ID" 0 first_automation_id;
  Alcotest.(check int) "first device ID" 0 first_device_id;

  (* Verify MainMixer tempo value *)
  let tempo_value = match main_track.mixer.tempo.Device.GenericParam.value with
    | Device.Float f -> f
    | _ -> 0.0
  in
  Alcotest.(check (float 0.001)) "tempo value" 120.0 tempo_value

let test_main_track_edge_case_empty () =
  (* Create a minimal main track XML with no automations or devices *)
  let empty_main_track_xml = Xml.Element {
      name = "MainTrack";
      attrs = [];
      childs = [
        Xml.Element { name = "LomId"; attrs = ["Value", "100"]; childs = [] };
        Xml.Element { name = "Name"; attrs = [];
                      childs = [Xml.Element { name = "EffectiveName"; attrs = ["Value", "Empty Main"]; childs = [] }] };
        Xml.Element { name = "AutomationEnvelopes"; attrs = [];
                      childs = [Xml.Element { name = "Envelopes"; attrs = []; childs = [] }] };
        Xml.Element { name = "DeviceChain"; attrs = [];
                      childs = [
                        Xml.Element { name = "Devices"; attrs = []; childs = [] };
                        Xml.Element { name = "Mixer"; attrs = [];
                                      childs = [
                                        Xml.Element { name = "Volume"; attrs = [];
                                                      childs = [Xml.Element { name = "Manual"; attrs = ["Value", "1.0"]; childs = [] }] };
                                        Xml.Element { name = "Pan"; attrs = [];
                                                      childs = [Xml.Element { name = "Manual"; attrs = ["Value", "0.0"]; childs = [] }] };
                                        Xml.Element { name = "On"; attrs = [];
                                                      childs = [Xml.Element { name = "Manual"; attrs = ["Value", "true"]; childs = [] }] };
                                        Xml.Element { name = "SoloSink"; attrs = ["Value", "false"]; childs = [] };
                                        Xml.Element { name = "Sends"; attrs = []; childs = [] };
                                        (* MainMixer specific parameters *)
                                        Xml.Element { name = "Tempo"; attrs = [];
                                                      childs = [Xml.Element { name = "Manual"; attrs = ["Value", "120"]; childs = [] }] };
                                        Xml.Element { name = "TimeSignature"; attrs = [];
                                                      childs = [Xml.Element { name = "Manual"; attrs = ["Value", "201"]; childs = [] }] };
                                        Xml.Element { name = "CrossFade"; attrs = [];
                                                      childs = [Xml.Element { name = "Manual"; attrs = ["Value", "0"]; childs = [] }] };
                                        Xml.Element { name = "GlobalGrooveAmount"; attrs = [];
                                                      childs = [Xml.Element { name = "Manual"; attrs = ["Value", "100"]; childs = [] }] }
                                      ] };
                        Xml.Element { name = "AudioInputRouting"; attrs = []; childs = [
                            Xml.Element { name = "Target"; attrs = ["Value", ""]; childs = [] };
                            Xml.Element { name = "UpperDisplayString"; attrs = ["Value", ""]; childs = [] };
                            Xml.Element { name = "LowerDisplayString"; attrs = ["Value", ""]; childs = [] }
                          ] };
                        Xml.Element { name = "AudioOutputRouting"; attrs = []; childs = [
                            Xml.Element { name = "Target"; attrs = ["Value", ""]; childs = [] };
                            Xml.Element { name = "UpperDisplayString"; attrs = ["Value", ""]; childs = [] };
                            Xml.Element { name = "LowerDisplayString"; attrs = ["Value", ""]; childs = [] }
                          ] };
                        Xml.Element { name = "MidiInputRouting"; attrs = []; childs = [
                            Xml.Element { name = "Target"; attrs = ["Value", ""]; childs = [] };
                            Xml.Element { name = "UpperDisplayString"; attrs = ["Value", ""]; childs = [] };
                            Xml.Element { name = "LowerDisplayString"; attrs = ["Value", ""]; childs = [] }
                          ] };
                        Xml.Element { name = "MidiOutputRouting"; attrs = []; childs = [
                            Xml.Element { name = "Target"; attrs = ["Value", ""]; childs = [] };
                            Xml.Element { name = "UpperDisplayString"; attrs = ["Value", ""]; childs = [] };
                            Xml.Element { name = "LowerDisplayString"; attrs = ["Value", ""]; childs = [] }
                          ] }
                      ] }
      ]
    } in

  let main_track = MainTrack.create empty_main_track_xml in
  Alcotest.(check string) "empty main track name" "Empty Main" main_track.name;
  Alcotest.(check int) "empty main track automation count" 0 (List.length main_track.automations);
  Alcotest.(check int) "empty main track device count" 0 (List.length main_track.devices)

let ts_eq = Alsdiff_live.Clip.TimeSignature.equal

let test_decode_time_signature_44 () =
  (* 44to114: values 200..208 map to 3/4 .. 11/4 *)
  let checks = [
    (200, 3, 4); (201, 4, 4); (202, 5, 4); (203, 6, 4);
    (204, 7, 4); (205, 8, 4); (206, 9, 4); (207, 10, 4); (208, 11, 4);
  ] in
  List.iter (fun (code, numer, denom) ->
      let ts = MainTrack.decode_time_signature code in
      Alcotest.(check bool) (Printf.sprintf "%d -> %d/%d" code numer denom)
        true (ts_eq ts { Alsdiff_live.Clip.TimeSignature.numer; denom })
    ) checks

let test_decode_time_signature_48 () =
  (* 48to138: values 299..309 map to 3/8 .. 13/8 *)
  let checks = [
    (299, 3, 8); (300, 4, 8); (301, 5, 8); (302, 6, 8);
    (303, 7, 8); (304, 8, 8); (305, 9, 8); (306, 10, 8);
    (307, 11, 8); (308, 12, 8); (309, 13, 8);
  ] in
  List.iter (fun (code, numer, denom) ->
      let ts = MainTrack.decode_time_signature code in
      Alcotest.(check bool) (Printf.sprintf "%d -> %d/%d" code numer denom)
        true (ts_eq ts { Alsdiff_live.Clip.TimeSignature.numer; denom })
    ) checks

let test_time_to_position_48to138 () =
  (* 48to138 project: time signature automation from 4/8 through 13/8 *)
  let open Alsdiff_live.Clip.TimeSignature in
  let events = [
    (-63072000.0, { numer = 4; denom = 8 });
    (16.0, { numer = 3; denom = 8 });
    (40.0, { numer = 4; denom = 8 });
    (72.0, { numer = 5; denom = 8 });
    (92.0, { numer = 6; denom = 8 });
    (116.0, { numer = 7; denom = 8 });
    (144.0, { numer = 8; denom = 8 });
    (176.0, { numer = 9; denom = 8 });
    (212.0, { numer = 10; denom = 8 });
    (232.0, { numer = 11; denom = 8 });
    (254.0, { numer = 12; denom = 8 });
    (278.0, { numer = 13; denom = 8 });
  ] in
  let check_pos time (exp_bar, exp_beat, exp_sixteenth) =
    let bar, beat, sixteenth = MainTrack.time_to_position events time in
    let label = Printf.sprintf "Time=%.0f -> (%d,%d,%d)" time exp_bar exp_beat exp_sixteenth in
    Alcotest.(check int) (label ^ " bar") exp_bar bar;
    Alcotest.(check int) (label ^ " beat") exp_beat beat;
    Alcotest.(check int) (label ^ " sixteenth") exp_sixteenth sixteenth
  in
  check_pos 16.0 (9, 1, 1);
  check_pos 40.0 (25, 1, 1);
  check_pos 72.0 (41, 1, 1);
  check_pos 92.0 (49, 1, 1);
  check_pos 116.0 (57, 1, 1);
  check_pos 144.0 (65, 1, 1);
  check_pos 176.0 (73, 1, 1);
  check_pos 212.0 (81, 1, 1);
  check_pos 232.0 (85, 1, 1);
  check_pos 254.0 (89, 1, 1);
  check_pos 278.0 (93, 1, 1)

let () =
  Alcotest.run "MainTrack" [
    "track_creation", [
      Alcotest.test_case "parse basic MainTrack properties" `Quick test_main_track_basic_properties;
      Alcotest.test_case "parse MainTrack automations" `Quick test_main_track_automations;
      Alcotest.test_case "parse MainTrack devices" `Quick test_main_track_devices;
      Alcotest.test_case "parse MainTrack mixer properties" `Quick test_main_track_mixer;
      Alcotest.test_case "parse MainTrack routing configuration" `Quick test_main_track_routing;
      Alcotest.test_case "comprehensive MainTrack parsing" `Quick test_main_track_comprehensive;
      Alcotest.test_case "handle empty track edge case" `Quick test_main_track_edge_case_empty;
    ];
    "decode_time_signature", [
      Alcotest.test_case "decode /4 time signatures (200..208)" `Quick test_decode_time_signature_44;
      Alcotest.test_case "decode /8 time signatures (299..309)" `Quick test_decode_time_signature_48;
    ];
    "time_to_position", [
      Alcotest.test_case "48to138 bar-boundary mappings" `Quick test_time_to_position_48to138;
    ]
  ]
