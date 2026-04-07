open Alsdiff_base
open Alsdiff_live


let test_invalid_xml_element_name () =
  (* Create an invalid XML element with unsupported track type *)
  let invalid_xml = Xml.Element {
      name = "InvalidTrackType";
      attrs = ["Id", "123"];
      childs = [];
    } in

  (* Track.create should raise Xml_error for unsupported track type *)
  let exception_thrown = try
      let _ = Track.create invalid_xml in
      false
    with
    | Xml.Xml_error _ -> true
    | _ -> false
  in

  Alcotest.(check bool) "invalid element name raises Xml_error" true exception_thrown


let test_data_node_error () =
  (* Create a Data node instead of Element *)
  let data_xml = Xml.Data "some data" in

  (* Track.create should raise Xml_error for Data node *)
  let exception_thrown = try
      let _ = Track.create data_xml in
      false
    with
    | Xml.Xml_error _ -> true
    | _ -> false
  in

  Alcotest.(check bool) "data node raises Xml_error" true exception_thrown


let test_midi_track_missing_id_attribute () =
  (* Create a MidiTrack element without Id attribute *)
  let invalid_xml = Xml.Element {
      name = "MidiTrack";
      attrs = []; (* Missing Id attribute *)
      childs = [
        (* Minimal valid child structure *)
        Xml.Element {
          name = "Name";
          attrs = [];
          childs = [
            Xml.Element {
              name = "EffectiveName";
              attrs = ["Value", "Test Track"];
              childs = [];
            }
          ];
        };
        Xml.Element {
          name = "DeviceChain";
          attrs = [];
          childs = [
            (* Mixer *)
            Xml.Element {
              name = "Mixer";
              attrs = [];
              childs = [
                Xml.Element {
                  name = "Volume";
                  attrs = [];
                  childs = [
                    Xml.Element {
                      name = "Manual";
                      attrs = ["Value", "1.0"];
                      childs = [];
                    }
                  ];
                };
                Xml.Element {
                  name = "Pan";
                  attrs = [];
                  childs = [
                    Xml.Element {
                      name = "Manual";
                      attrs = ["Value", "0.0"];
                      childs = [];
                    }
                  ];
                };
                Xml.Element {
                  name = "On";
                  attrs = [];
                  childs = [
                    Xml.Element {
                      name = "Manual";
                      attrs = ["Value", "true"];
                      childs = [];
                    }
                  ];
                };
                Xml.Element {
                  name = "SoloSink";
                  attrs = ["Value", "false"];
                  childs = [];
                };
              ];
            };
            (* Routings - minimal routing elements *)
            Xml.Element {
              name = "AudioInputRouting";
              attrs = [];
              childs = [
                Xml.Element { name = "Target"; attrs = ["Value", ""]; childs = []; };
                Xml.Element { name = "UpperDisplayString"; attrs = ["Value", ""]; childs = []; };
                Xml.Element { name = "LowerDisplayString"; attrs = ["Value", ""]; childs = []; };
              ];
            };
            Xml.Element {
              name = "AudioOutputRouting";
              attrs = [];
              childs = [
                Xml.Element { name = "Target"; attrs = ["Value", ""]; childs = []; };
                Xml.Element { name = "UpperDisplayString"; attrs = ["Value", ""]; childs = []; };
                Xml.Element { name = "LowerDisplayString"; attrs = ["Value", ""]; childs = []; };
              ];
            };
            Xml.Element {
              name = "MidiInputRouting";
              attrs = [];
              childs = [
                Xml.Element { name = "Target"; attrs = ["Value", ""]; childs = []; };
                Xml.Element { name = "UpperDisplayString"; attrs = ["Value", ""]; childs = []; };
                Xml.Element { name = "LowerDisplayString"; attrs = ["Value", ""]; childs = []; };
              ];
            };
            Xml.Element {
              name = "MidiOutputRouting";
              attrs = [];
              childs = [
                Xml.Element { name = "Target"; attrs = ["Value", ""]; childs = []; };
                Xml.Element { name = "UpperDisplayString"; attrs = ["Value", ""]; childs = []; };
                Xml.Element { name = "LowerDisplayString"; attrs = ["Value", ""]; childs = []; };
              ];
            };
          ];
        };
      ];
    } in

  (* Track.create should raise an error when Id attribute is missing *)
  let exception_thrown = try
      let _ = Track.create invalid_xml in
      false
    with
    | _ -> true (* Any exception is acceptable for missing required attribute *)
  in

  Alcotest.(check bool) "missing Id attribute raises exception" true exception_thrown


let test_audio_track_missing_name_element () =
  (* Create an AudioTrack without Name element *)
  let invalid_xml = Xml.Element {
      name = "AudioTrack";
      attrs = ["Id", "100"];
      childs = [
        Xml.Element {
          name = "DeviceChain";
          attrs = [];
          childs = [
            (* Mixer - minimal *)
            Xml.Element {
              name = "Mixer";
              attrs = [];
              childs = [
                Xml.Element {
                  name = "Volume";
                  attrs = [];
                  childs = [
                    Xml.Element {
                      name = "Manual";
                      attrs = ["Value", "1.0"];
                      childs = [];
                    }
                  ];
                };
                Xml.Element {
                  name = "Pan";
                  attrs = [];
                  childs = [
                    Xml.Element {
                      name = "Manual";
                      attrs = ["Value", "0.0"];
                      childs = [];
                    }
                  ];
                };
                Xml.Element {
                  name = "On";
                  attrs = [];
                  childs = [
                    Xml.Element {
                      name = "Manual";
                      attrs = ["Value", "true"];
                      childs = [];
                    }
                  ];
                };
                Xml.Element {
                  name = "SoloSink";
                  attrs = ["Value", "false"];
                  childs = [];
                };
              ];
            };
            (* Minimal routings *)
            Xml.Element {
              name = "AudioInputRouting";
              attrs = [];
              childs = [
                Xml.Element { name = "Target"; attrs = ["Value", ""]; childs = []; };
                Xml.Element { name = "UpperDisplayString"; attrs = ["Value", ""]; childs = []; };
                Xml.Element { name = "LowerDisplayString"; attrs = ["Value", ""]; childs = []; };
              ];
            };
            Xml.Element {
              name = "AudioOutputRouting";
              attrs = [];
              childs = [
                Xml.Element { name = "Target"; attrs = ["Value", ""]; childs = []; };
                Xml.Element { name = "UpperDisplayString"; attrs = ["Value", ""]; childs = []; };
                Xml.Element { name = "LowerDisplayString"; attrs = ["Value", ""]; childs = []; };
              ];
            };
            Xml.Element {
              name = "MidiInputRouting";
              attrs = [];
              childs = [
                Xml.Element { name = "Target"; attrs = ["Value", ""]; childs = []; };
                Xml.Element { name = "UpperDisplayString"; attrs = ["Value", ""]; childs = []; };
                Xml.Element { name = "LowerDisplayString"; attrs = ["Value", ""]; childs = []; };
              ];
            };
            Xml.Element {
              name = "MidiOutputRouting";
              attrs = [];
              childs = [
                Xml.Element { name = "Target"; attrs = ["Value", ""]; childs = []; };
                Xml.Element { name = "UpperDisplayString"; attrs = ["Value", ""]; childs = []; };
                Xml.Element { name = "LowerDisplayString"; attrs = ["Value", ""]; childs = []; };
              ];
            };
          ];
        };
      ];
    } in

  (* Track.create should raise an error when Name element is missing *)
  let exception_thrown = try
      let _ = Track.create invalid_xml in
      false
    with
    | _ -> true (* Any exception is acceptable for missing required element *)
  in

  Alcotest.(check bool) "missing Name element raises exception" true exception_thrown


let test_malformed_xml_attributes () =
  (* Create XML with non-integer Id attribute *)
  let invalid_xml = Xml.Element {
      name = "MidiTrack";
      attrs = ["Id", "not_an_integer"];
      childs = [
        Xml.Element {
          name = "Name";
          attrs = [];
          childs = [
            Xml.Element {
              name = "EffectiveName";
              attrs = ["Value", "Test Track"];
              childs = [];
            }
          ];
        };
        Xml.Element {
          name = "DeviceChain";
          attrs = [];
          childs = [
            (* Minimal mixer *)
            Xml.Element {
              name = "Mixer";
              attrs = [];
              childs = [
                Xml.Element {
                  name = "Volume";
                  attrs = [];
                  childs = [
                    Xml.Element {
                      name = "Manual";
                      attrs = ["Value", "1.0"];
                      childs = [];
                    }
                  ];
                };
                Xml.Element {
                  name = "Pan";
                  attrs = [];
                  childs = [
                    Xml.Element {
                      name = "Manual";
                      attrs = ["Value", "0.0"];
                      childs = [];
                    }
                  ];
                };
                Xml.Element {
                  name = "On";
                  attrs = [];
                  childs = [
                    Xml.Element {
                      name = "Manual";
                      attrs = ["Value", "true"];
                      childs = [];
                    }
                  ];
                };
                Xml.Element {
                  name = "SoloSink";
                  attrs = ["Value", "false"];
                  childs = [];
                };
              ];
            };
            (* Minimal routings *)
            Xml.Element {
              name = "AudioInputRouting";
              attrs = [];
              childs = [
                Xml.Element { name = "Target"; attrs = ["Value", ""]; childs = []; };
                Xml.Element { name = "UpperDisplayString"; attrs = ["Value", ""]; childs = []; };
                Xml.Element { name = "LowerDisplayString"; attrs = ["Value", ""]; childs = []; };
              ];
            };
            Xml.Element {
              name = "AudioOutputRouting";
              attrs = [];
              childs = [
                Xml.Element { name = "Target"; attrs = ["Value", ""]; childs = []; };
                Xml.Element { name = "UpperDisplayString"; attrs = ["Value", ""]; childs = []; };
                Xml.Element { name = "LowerDisplayString"; attrs = ["Value", ""]; childs = []; };
              ];
            };
            Xml.Element {
              name = "MidiInputRouting";
              attrs = [];
              childs = [
                Xml.Element { name = "Target"; attrs = ["Value", ""]; childs = []; };
                Xml.Element { name = "UpperDisplayString"; attrs = ["Value", ""]; childs = []; };
                Xml.Element { name = "LowerDisplayString"; attrs = ["Value", ""]; childs = []; };
              ];
            };
            Xml.Element {
              name = "MidiOutputRouting";
              attrs = [];
              childs = [
                Xml.Element { name = "Target"; attrs = ["Value", ""]; childs = []; };
                Xml.Element { name = "UpperDisplayString"; attrs = ["Value", ""]; childs = []; };
                Xml.Element { name = "LowerDisplayString"; attrs = ["Value", ""]; childs = []; };
              ];
            };
          ];
        };
      ];
    } in

  (* Track.create should raise an error when Id is not an integer *)
  let exception_thrown = try
      let _ = Track.create invalid_xml in
      false
    with
    | _ -> true (* Any exception is acceptable for malformed attribute *)
  in

  Alcotest.(check bool) "non-integer Id raises exception" true exception_thrown


let test_empty_track_type_variants () =
  (* Test that different track type variants are correctly identified *)

  (* Create minimal valid MidiTrack XML *)
  let make_minimal_track ~name ~id () =
    Xml.Element {
      name;
      attrs = ["Id", string_of_int id];
      childs = [
        Xml.Element {
          name = "Name";
          attrs = [];
          childs = [
            Xml.Element {
              name = "EffectiveName";
              attrs = ["Value", "Test"];
              childs = [];
            }
          ];
        };
        Xml.Element {
          name = "DeviceChain";
          attrs = [];
          childs = [
            (* Minimal mixer *)
            Xml.Element {
              name = "Mixer";
              attrs = [];
              childs = [
                Xml.Element {
                  name = "Volume";
                  attrs = [];
                  childs = [
                    Xml.Element {
                      name = "Manual";
                      attrs = ["Value", "1.0"];
                      childs = [];
                    }
                  ];
                };
                Xml.Element {
                  name = "Pan";
                  attrs = [];
                  childs = [
                    Xml.Element {
                      name = "Manual";
                      attrs = ["Value", "0.0"];
                      childs = [];
                    }
                  ];
                };
                Xml.Element {
                  name = "On";
                  attrs = [];
                  childs = [
                    Xml.Element {
                      name = "Manual";
                      attrs = ["Value", "true"];
                      childs = [];
                    }
                  ];
                };
                Xml.Element {
                  name = "SoloSink";
                  attrs = ["Value", "false"];
                  childs = [];
                };
              ];
            };
            (* Minimal routings *)
            Xml.Element {
              name = "AudioInputRouting";
              attrs = [];
              childs = [
                Xml.Element { name = "Target"; attrs = ["Value", ""]; childs = []; };
                Xml.Element { name = "UpperDisplayString"; attrs = ["Value", ""]; childs = []; };
                Xml.Element { name = "LowerDisplayString"; attrs = ["Value", ""]; childs = []; };
              ];
            };
            Xml.Element {
              name = "AudioOutputRouting";
              attrs = [];
              childs = [
                Xml.Element { name = "Target"; attrs = ["Value", ""]; childs = []; };
                Xml.Element { name = "UpperDisplayString"; attrs = ["Value", ""]; childs = []; };
                Xml.Element { name = "LowerDisplayString"; attrs = ["Value", ""]; childs = []; };
              ];
            };
            Xml.Element {
              name = "MidiInputRouting";
              attrs = [];
              childs = [
                Xml.Element { name = "Target"; attrs = ["Value", ""]; childs = []; };
                Xml.Element { name = "UpperDisplayString"; attrs = ["Value", ""]; childs = []; };
                Xml.Element { name = "LowerDisplayString"; attrs = ["Value", ""]; childs = []; };
              ];
            };
            Xml.Element {
              name = "MidiOutputRouting";
              attrs = [];
              childs = [
                Xml.Element { name = "Target"; attrs = ["Value", ""]; childs = []; };
                Xml.Element { name = "UpperDisplayString"; attrs = ["Value", ""]; childs = []; };
                Xml.Element { name = "LowerDisplayString"; attrs = ["Value", ""]; childs = []; };
              ];
            };
          ];
        };
      ];
    }
  in

  (* Test each track type variant *)
  let midi_track = make_minimal_track ~name:"MidiTrack" ~id:1 () in
  let audio_track = make_minimal_track ~name:"AudioTrack" ~id:2 () in
  let group_track = make_minimal_track ~name:"GroupTrack" ~id:3 () in
  let return_track = make_minimal_track ~name:"ReturnTrack" ~id:4 () in

  (* MainTrack requires additional MainMixer fields - create it separately *)
  let main_track = Xml.Element {
      name = "MainTrack";
      attrs = [];
      childs = [
        Xml.Element {
          name = "Name";
          attrs = [];
          childs = [
            Xml.Element {
              name = "EffectiveName";
              attrs = ["Value", "Master"];
              childs = [];
            }
          ];
        };
        Xml.Element {
          name = "DeviceChain";
          attrs = [];
          childs = [
            (* MainMixer with Tempo/TimeSignature/etc *)
            Xml.Element {
              name = "Mixer";
              attrs = [];
              childs = [
                Xml.Element {
                  name = "Volume";
                  attrs = [];
                  childs = [
                    Xml.Element {
                      name = "Manual";
                      attrs = ["Value", "1.0"];
                      childs = [];
                    }
                  ];
                };
                Xml.Element {
                  name = "Pan";
                  attrs = [];
                  childs = [
                    Xml.Element {
                      name = "Manual";
                      attrs = ["Value", "0.0"];
                      childs = [];
                    }
                  ];
                };
                Xml.Element {
                  name = "On";
                  attrs = [];
                  childs = [
                    Xml.Element {
                      name = "Manual";
                      attrs = ["Value", "true"];
                      childs = [];
                    }
                  ];
                };
                Xml.Element {
                  name = "SoloSink";
                  attrs = ["Value", "false"];
                  childs = [];
                };
                (* MainMixer-specific fields *)
                Xml.Element {
                  name = "Tempo";
                  attrs = [];
                  childs = [
                    Xml.Element {
                      name = "Manual";
                      attrs = ["Value", "120.0"];
                      childs = [];
                    }
                  ];
                };
                Xml.Element {
                  name = "TimeSignature";
                  attrs = [];
                  childs = [
                    Xml.Element {
                      name = "Manual";
                      attrs = ["Value", "4"];
                      childs = [];
                    }
                  ];
                };
                Xml.Element {
                  name = "CrossFade";
                  attrs = [];
                  childs = [
                    Xml.Element {
                      name = "Manual";
                      attrs = ["Value", "1.0"];
                      childs = [];
                    }
                  ];
                };
                Xml.Element {
                  name = "GlobalGrooveAmount";
                  attrs = [];
                  childs = [
                    Xml.Element {
                      name = "Manual";
                      attrs = ["Value", "0.0"];
                      childs = [];
                    }
                  ];
                };
              ];
            };
            (* Minimal routings *)
            Xml.Element {
              name = "AudioInputRouting";
              attrs = [];
              childs = [
                Xml.Element { name = "Target"; attrs = ["Value", ""]; childs = []; };
                Xml.Element { name = "UpperDisplayString"; attrs = ["Value", ""]; childs = []; };
                Xml.Element { name = "LowerDisplayString"; attrs = ["Value", ""]; childs = []; };
              ];
            };
            Xml.Element {
              name = "AudioOutputRouting";
              attrs = [];
              childs = [
                Xml.Element { name = "Target"; attrs = ["Value", ""]; childs = []; };
                Xml.Element { name = "UpperDisplayString"; attrs = ["Value", ""]; childs = []; };
                Xml.Element { name = "LowerDisplayString"; attrs = ["Value", ""]; childs = []; };
              ];
            };
            Xml.Element {
              name = "MidiInputRouting";
              attrs = [];
              childs = [
                Xml.Element { name = "Target"; attrs = ["Value", ""]; childs = []; };
                Xml.Element { name = "UpperDisplayString"; attrs = ["Value", ""]; childs = []; };
                Xml.Element { name = "LowerDisplayString"; attrs = ["Value", ""]; childs = []; };
              ];
            };
            Xml.Element {
              name = "MidiOutputRouting";
              attrs = [];
              childs = [
                Xml.Element { name = "Target"; attrs = ["Value", ""]; childs = []; };
                Xml.Element { name = "UpperDisplayString"; attrs = ["Value", ""]; childs = []; };
                Xml.Element { name = "LowerDisplayString"; attrs = ["Value", ""]; childs = []; };
              ];
            };
          ];
        };
      ];
    } in

  (* Verify each creates the correct variant *)
  (match Track.create midi_track with
   | Track.Midi _ -> ()
   | _ -> Alcotest.fail "MidiTrack should create Midi variant");

  (match Track.create audio_track with
   | Track.Audio _ -> ()
   | _ -> Alcotest.fail "AudioTrack should create Audio variant");

  (match Track.create group_track with
   | Track.Group _ -> ()
   | _ -> Alcotest.fail "GroupTrack should create Group variant");

  (match Track.create return_track with
   | Track.Return _ -> ()
   | _ -> Alcotest.fail "ReturnTrack should create Return variant");

  (match Track.create main_track with
   | Track.Main _ -> ()
   | _ -> Alcotest.fail "MainTrack should create Main variant")


let () =
  Alcotest.run "Track Error Handling" [
    "invalid_xml", [
      Alcotest.test_case "invalid XML element name raises error" `Quick test_invalid_xml_element_name;
      Alcotest.test_case "data node raises error" `Quick test_data_node_error;
      Alcotest.test_case "malformed XML attributes raises error" `Quick test_malformed_xml_attributes;
    ];
    "missing_elements", [
      Alcotest.test_case "missing Id attribute raises error" `Quick test_midi_track_missing_id_attribute;
      Alcotest.test_case "missing Name element raises error" `Quick test_audio_track_missing_name_element;
    ];
    "edge_cases", [
      Alcotest.test_case "empty track type variants" `Quick test_empty_track_type_variants;
    ];
  ]
