open Alsdiff_base.Xml
open Alsdiff_live

let test_device_param_with_missing_values () =
  (* Create a parameter XML structure with missing values *)
  let param_xml = Element {
      name = "TestParam";
      attrs = [("Id", "101")];
      childs = [
        Element {
          name = "LomId";
          attrs = [("Value", "0")];
          childs = [];
        };
        Element {
          name = "Manual";
          attrs = [("Value", "0.0")];
          childs = [];
        }
        (* Missing AutomationTarget element - should default to 0 *)
      ];
    } in

  (* Create a parameter from the XML *)
  let open Device.DeviceParam in
  let param = create "TestParam" param_xml in

  (* Verify parameter properties with defaults *)
  Alcotest.(check string) "param name" "TestParam" param.base.Device.GenericParam.name;
  (match param.base.Device.GenericParam.value with
   | Float v -> Alcotest.(check (float 0.01)) "param value (default)" 0.0 v
   | _ -> Alcotest.fail "parameter should be float");

  Alcotest.(check int) "param automation id (default)" 0 param.base.Device.GenericParam.automation;
  Alcotest.(check int) "param modulation id (default)" 0 param.base.Device.GenericParam.modulation

let test_device_creation_with_invalid_xml () =
  (* Create invalid XML (Data instead of Element) *)
  let invalid_xml = Data "invalid" in

  (* This should raise an exception *)
  (try ignore (Device.RegularDevice.create invalid_xml); false
   with Alsdiff_base.Xml.Xml_error (_, msg) when msg = "Invalid XML element for creating Device" -> true
      | _ -> false)
  |> Alcotest.(check bool) "invalid xml raises exception" true

let test_param_creation_with_invalid_xml () =
  (* Create invalid XML (Data instead of Element) *)
  let invalid_xml = Data "invalid" in

  (* This should raise an exception *)
  (try ignore (Device.DeviceParam.create "test" invalid_xml); false
   with Alsdiff_base.Xml.Xml_error (_, msg) when msg = "Invalid XML element for creating DeviceParam" -> true
      | _ -> false)
  |> Alcotest.(check bool) "invalid xml raises exception" true

let test_device_param_with_continuous_macro_mapping () =
  (* Create a parameter XML structure with continuous macro mapping *)
  let param_xml = Element {
      name = "Coarse";
      attrs = [("Id", "100")];
      childs = [
        Element {
          name = "LomId";
          attrs = [("Value", "0")];
          childs = [];
        };
        Element {
          name = "KeyMidi";
          attrs = [];
          childs = [
            Element {
              name = "PersistentKeyString";
              attrs = [("Value", "")];
              childs = [];
            };
            Element {
              name = "IsNote";
              attrs = [("Value", "true")];
              childs = [];
            };
            Element {
              name = "Channel";
              attrs = [("Value", "16")];
              childs = [];
            };
            Element {
              name = "NoteOrController";
              attrs = [("Value", "3")];
              childs = [];
            };
            Element {
              name = "LowerRangeNote";
              attrs = [("Value", "-1")];
              childs = [];
            };
            Element {
              name = "UpperRangeNote";
              attrs = [("Value", "-1")];
              childs = [];
            };
            Element {
              name = "ControllerMapMode";
              attrs = [("Value", "1")];
              childs = [];
            };
          ];
        };
        Element {
          name = "Manual";
          attrs = [("Value", "31")];
          childs = [];
        };
        Element {
          name = "MidiControllerRange";
          attrs = [];
          childs = [
            Element {
              name = "Min";
              attrs = [("Value", "0")];
              childs = [];
            };
            Element {
              name = "Max";
              attrs = [("Value", "48")];
              childs = [];
            };
          ];
        };
        Element {
          name = "AutomationTarget";
          attrs = [("Id", "200")];
          childs = [
            Element {
              name = "LockEnvelope";
              attrs = [("Value", "0")];
              childs = [];
            }
          ];
        }
      ];
    } in

  (* Create a parameter from the XML *)
  let open Device.DeviceParam in
  let param = create "Coarse" param_xml in

  (* Verify parameter properties *)
  Alcotest.(check string) "param name" "Coarse" param.base.Device.GenericParam.name;
  (match param.base.Device.GenericParam.value with
   | Float v -> Alcotest.(check (float 0.01)) "param value" 31.0 v
   | _ -> Alcotest.fail "parameter should be float");

  Alcotest.(check int) "param automation id" 200 param.base.Device.GenericParam.automation;
  Alcotest.(check int) "param modulation id" 0 param.base.Device.GenericParam.modulation;
  (* Verify macro mapping *)
  (match param.base.Device.GenericParam.mapping with
   | Some mapping -> (
       Alcotest.(check int) "macro id" 3 mapping.target;
       Alcotest.(check int) "macro range low" 0 mapping.low;
       Alcotest.(check int) "macro range high" 48 mapping.high
     )
   | None -> Alcotest.fail "parameter should have macro mapping");
  ()

let test_device_param_with_onoff_macro_mapping () =
  (* Create a parameter XML structure with On/Off macro mapping *)
  let param_xml = Element {
      name = "IsOn";
      attrs = [("Id", "101")];
      childs = [
        Element {
          name = "LomId";
          attrs = [("Value", "0")];
          childs = [];
        };
        Element {
          name = "KeyMidi";
          attrs = [];
          childs = [
            Element {
              name = "PersistentKeyString";
              attrs = [("Value", "")];
              childs = [];
            };
            Element {
              name = "IsNote";
              attrs = [("Value", "true")];
              childs = [];
            };
            Element {
              name = "Channel";
              attrs = [("Value", "16")];
              childs = [];
            };
            Element {
              name = "NoteOrController";
              attrs = [("Value", "0")];
              childs = [];
            };
            Element {
              name = "LowerRangeNote";
              attrs = [("Value", "-1")];
              childs = [];
            };
            Element {
              name = "UpperRangeNote";
              attrs = [("Value", "-1")];
              childs = [];
            };
            Element {
              name = "ControllerMapMode";
              attrs = [("Value", "1")];
              childs = [];
            };
          ];
        };
        Element {
          name = "Manual";
          attrs = [("Value", "true")];
          childs = [];
        };
        Element {
          name = "MidiCCOnOffThresholds";
          attrs = [];
          childs = [
            Element {
              name = "Min";
              attrs = [("Value", "64")];
              childs = [];
            };
            Element {
              name = "Max";
              attrs = [("Value", "127")];
              childs = [];
            };
          ];
        };
        Element {
          name = "AutomationTarget";
          attrs = [("Id", "201")];
          childs = [
            Element {
              name = "LockEnvelope";
              attrs = [("Value", "0")];
              childs = [];
            }
          ];
        }
      ];
    } in

  (* Create a parameter from the XML *)
  let open Device.DeviceParam in
  let param = create "IsOn" param_xml in

  (* Verify parameter properties *)
  Alcotest.(check string) "param name" "IsOn" param.base.Device.GenericParam.name;
  (match param.base.Device.GenericParam.value with
   | Bool v -> Alcotest.(check bool) "param value" true v
   | _ -> Alcotest.fail "parameter should be bool");

  Alcotest.(check int) "param automation id" 201 param.base.Device.GenericParam.automation;
  Alcotest.(check int) "param modulation id" 0 param.base.Device.GenericParam.modulation;
  (* Verify macro mapping *)
  (match param.base.Device.GenericParam.mapping with
   | Some mapping -> (
       Alcotest.(check int) "macro id" 0 mapping.target;
       Alcotest.(check int) "macro range low" 64 mapping.low;
       Alcotest.(check int) "macro range high" 127 mapping.high
     )
   | None -> Alcotest.fail "parameter should have macro mapping");
  ()


let test_device_param_no_macro_when_is_note_false () =
  (* Create a parameter XML structure with IsNote=false and ControllerMapMode=1 *)
  let param_xml = Element {
      name = "NonMacroParam";
      attrs = [("Id", "103")];
      childs = [
        Element {
          name = "LomId";
          attrs = [("Value", "0")];
          childs = [];
        };
        Element {
          name = "KeyMidi";
          attrs = [];
          childs = [
            Element {
              name = "PersistentKeyString";
              attrs = [("Value", "")];
              childs = [];
            };
            Element {
              name = "IsNote";
              attrs = [("Value", "false")];
              childs = [];
            };
            Element {
              name = "Channel";
              attrs = [("Value", "16")];
              childs = [];
            };
            Element {
              name = "NoteOrController";
              attrs = [("Value", "1")];
              childs = [];
            };
            Element {
              name = "ControllerMapMode";
              attrs = [("Value", "1")]; (* IsNote is false, so no macro mapping regardless of ControllerMapMode *)
              childs = [];
            };
          ];
        };
        Element {
          name = "Manual";
          attrs = [("Value", "0.5")];
          childs = [];
        };
        Element {
          name = "AutomationTarget";
          attrs = [("Id", "203")];
          childs = [
            Element {
              name = "LockEnvelope";
              attrs = [("Value", "0")];
              childs = [];
            }
          ];
        }
      ];
    } in

  (* Create a parameter from the XML *)
  let open Device.DeviceParam in
  let param = create "NonMacroParam" param_xml in

  (* Verify parameter properties *)
  Alcotest.(check string) "param name" "NonMacroParam" param.base.Device.GenericParam.name;
  (match param.base.Device.GenericParam.value with
   | Float v -> Alcotest.(check (float 0.01)) "param value" 0.5 v
   | _ -> Alcotest.fail "parameter should be float");

  Alcotest.(check int) "param automation id" 203 param.base.Device.GenericParam.automation;
  Alcotest.(check int) "param modulation id" 0 param.base.Device.GenericParam.modulation;
  (* Verify no macro mapping *)
  (match param.base.Device.GenericParam.mapping with
   | None -> () (* Expected - no macro mapping *)
   | Some _ -> Alcotest.fail "parameter should not have macro mapping")



let test_preset_ref_creation () =
  (* XML string from TODO.org example *)
  let preset_xml_str = {|
<FilePresetRef Id="0">
  <FileRef>
    <RelativePathType Value="6" />
    <RelativePath Value="Presets/Instruments/Instrument Rack/Industrial FM Kick.adg" />
    <Path Value="/Users/krfantasy/Music/Ableton/User Library/Presets/Instruments/Instrument Rack/Industrial FM Kick.adg" />
    <Type Value="2" />
    <LivePackName Value="" />
    <LivePackId Value="" />
    <OriginalFileSize Value="0" />
    <OriginalCrc Value="0" />
  </FileRef>
</FilePresetRef>
|} in

  (* Parse the XML string *)
  let xml = read_string preset_xml_str in

  (* Create a PresetRef from the XML *)
  let open Device.PresetRef in
  let preset_ref = create xml in

  (* Verify the PresetRef properties *)
  Alcotest.(check int) "preset id" 0 preset_ref.id;
  Alcotest.(check string) "relative path" "Presets/Instruments/Instrument Rack/Industrial FM Kick.adg" preset_ref.relative_path;
  Alcotest.(check string) "path" "/Users/krfantasy/Music/Ableton/User Library/Presets/Instruments/Instrument Rack/Industrial FM Kick.adg" preset_ref.path;
  Alcotest.(check string) "pack name" "" preset_ref.pack_name;
  Alcotest.(check int) "pack id" 0 preset_ref.pack_id;
  Alcotest.(check int) "file size" 0 preset_ref.file_size;
  Alcotest.(check int) "crc" 0 preset_ref.crc


let test_mixer_device_xml_path = Utils.resolve_test_data_path "mixer_device.xml"

let test_mixer_device_create () =
  let xml = read_file test_mixer_device_xml_path in
  let mixer = Device.MixerDevice.create xml in
  (* Verify On parameter *)
  (match mixer.on.base.Device.GenericParam.value with
   | Bool v -> Alcotest.(check bool) "on value" true v
   | _ -> Alcotest.fail "on should be bool");
  Alcotest.(check int) "on automation" 24267 mixer.on.base.Device.GenericParam.automation;
  (* Verify Speaker parameter *)
  (match mixer.speaker.base.Device.GenericParam.value with
   | Bool v -> Alcotest.(check bool) "speaker value" true v
   | _ -> Alcotest.fail "speaker should be bool");
  (* Verify Volume parameter *)
  (match mixer.volume.base.Device.GenericParam.value with
   | Float v -> Alcotest.(check (float 0.01)) "volume value" 1.0 v
   | _ -> Alcotest.fail "volume should be float");
  Alcotest.(check int) "volume automation" 24270 mixer.volume.base.Device.GenericParam.automation;
  Alcotest.(check int) "volume modulation" 24271 mixer.volume.base.Device.GenericParam.modulation;
  (* Verify Pan parameter *)
  (match mixer.pan.base.Device.GenericParam.value with
   | Float v -> Alcotest.(check (float 0.01)) "pan value" 0.0 v
   | _ -> Alcotest.fail "pan should be float");
  Alcotest.(check int) "pan automation" 24272 mixer.pan.base.Device.GenericParam.automation;
  Alcotest.(check int) "pan modulation" 24273 mixer.pan.base.Device.GenericParam.modulation

let test_mixer_device_has_same_id_and_hash () =
  let xml = read_file test_mixer_device_xml_path in
  let mixer1 = Device.MixerDevice.create xml in
  let mixer2 = Device.MixerDevice.create xml in
  (* has_same_id always returns true for MixerDevice *)
  Alcotest.(check bool) "has_same_id returns true" true (Device.MixerDevice.has_same_id mixer1 mixer2);
  (* id_hash always returns Hashtbl.hash 0 *)
  Alcotest.(check int) "id_hash consistent" (Device.MixerDevice.id_hash mixer1) (Device.MixerDevice.id_hash mixer2)

let test_mixer_device_diff_no_change () =
  let xml = read_file test_mixer_device_xml_path in
  let mixer = Device.MixerDevice.create xml in
  let patch = Device.MixerDevice.diff mixer mixer in
  Alcotest.(check bool) "patch is empty" true (Device.MixerDevice.Patch.is_empty patch)

let test_midi_mapping_create_head_key_midi () =
  let head_key_midi_xml = Element {
      name = "HeadKeyMidi";
      attrs = [];
      childs = [
        Element { name = "NoteOrController"; attrs = [("Value", "3")]; childs = [] };
        Element { name = "Channel"; attrs = [("Value", "16")]; childs = [] };
      ];
    } in
  let mapping = Device.MIDIMapping.create_head_key_midi head_key_midi_xml in
  Alcotest.(check int) "target" 3 mapping.target;
  Alcotest.(check int) "channel" 16 mapping.channel;
  (match mapping.kind with
   | OnOff -> ()
   | Continuous -> Alcotest.fail "expected OnOff kind");
  Alcotest.(check int) "low" 64 mapping.low;
  Alcotest.(check int) "high" 127 mapping.high

let test_generic_param_create_int_manual () =
  let param_xml = Element {
      name = "TestIntParam";
      attrs = [];
      childs = [
        Element { name = "Manual"; attrs = [("Value", "42")]; childs = [] };
      ];
    } in
  let param = Device.GenericParam.create_int_manual param_xml in
  Alcotest.(check string) "name" "TestIntParam" param.name;
  (match param.value with
   | Int v -> Alcotest.(check int) "int value" 42 v
   | _ -> Alcotest.fail "expected Int value");
  Alcotest.(check int) "automation default" 0 param.automation;
  Alcotest.(check int) "modulation default" 0 param.modulation

let () =
  Alcotest.run "Device" [
    "device_creation", [
      Alcotest.test_case "create parameter with missing values" `Quick test_device_param_with_missing_values;
      Alcotest.test_case "device creation with invalid XML" `Quick test_device_creation_with_invalid_xml;
      Alcotest.test_case "param creation with invalid XML" `Quick test_param_creation_with_invalid_xml;
      Alcotest.test_case "create PresetRef from XML" `Quick test_preset_ref_creation;
    ];
    "macro_mapping", [
      Alcotest.test_case "parameter with continuous macro mapping" `Quick test_device_param_with_continuous_macro_mapping;
      Alcotest.test_case "parameter with On/Off macro mapping" `Quick test_device_param_with_onoff_macro_mapping;
      Alcotest.test_case "parameter no macro when IsNote is false" `Quick test_device_param_no_macro_when_is_note_false;
    ];
    "mixer_device", [
      Alcotest.test_case "MixerDevice.create from XML" `Quick test_mixer_device_create;
      Alcotest.test_case "MixerDevice.has_same_id and id_hash" `Quick test_mixer_device_has_same_id_and_hash;
      Alcotest.test_case "MixerDevice.diff no change" `Quick test_mixer_device_diff_no_change;
    ];
    "midi_mapping", [
      Alcotest.test_case "MIDIMapping.create_head_key_midi" `Quick test_midi_mapping_create_head_key_midi;
    ];
    "generic_param", [
      Alcotest.test_case "GenericParam.create_int_manual" `Quick test_generic_param_create_int_manual;
    ];
  ]
