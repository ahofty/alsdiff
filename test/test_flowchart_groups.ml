open Alcotest
open Alsdiff_output

let contains_substring ~haystack ~needle =
  let len_h = String.length haystack in
  let len_n = String.length needle in
  let rec loop i =
    if i + len_n > len_h then false
    else if String.sub haystack i len_n = needle then true
    else loop (i + 1)
  in
  loop 0

let index_of_substring ~haystack ~needle =
  let len_h = String.length haystack in
  let len_n = String.length needle in
  let rec loop i =
    if i + len_n > len_h then None
    else if String.sub haystack i len_n = needle then Some i
    else loop (i + 1)
  in
  loop 0

let test_group_membership () =
  let open Flowchart in
  let track_info_map =
    IntMap.empty
    |> IntMap.add 64 { node = { id = "track_64"; label = "Group A (Group)" }; group_label = Some "Group A" }
    |> IntMap.add 12 { node = { id = "track_12"; label = "Child (MIDI)" }; group_label = None }
  in
  let track_parent =
    IntMap.empty
    |> IntMap.add 64 None
    |> IntMap.add 12 (Some 64)
  in
  let group_info = {
    track_parent;
    group_ids = IntSet.singleton 64;
    track_order = [64; 12];
  } in
  let output = Mermaid_renderer.render_nodes_with_groups
      ~direction:"LR"
      ~track_info_map
      ~main_node:{ id = "main"; label = "Main" }
      ~external_nodes:[]
      ~edges:[]
      ~group_info
  in
  check bool "subgraph present" true
    (contains_substring ~haystack:output ~needle:"subgraph group_64[\"Group A\"]");
  check bool "group node present" true
    (contains_substring ~haystack:output ~needle:"track_64[\"Group A (Group)\"]");
  check bool "child node present" true
    (contains_substring ~haystack:output ~needle:"track_12[\"Child (MIDI)\"]")

let test_nested_groups () =
  let open Flowchart in
  let track_info_map =
    IntMap.empty
    |> IntMap.add 10 { node = { id = "track_10"; label = "Top (Group)" }; group_label = Some "Top" }
    |> IntMap.add 11 { node = { id = "track_11"; label = "Inner (Group)" }; group_label = Some "Inner" }
    |> IntMap.add 12 { node = { id = "track_12"; label = "Child (Audio)" }; group_label = None }
    |> IntMap.add 20 { node = { id = "track_20"; label = "Solo (Audio)" }; group_label = None }
  in
  let track_parent =
    IntMap.empty
    |> IntMap.add 10 None
    |> IntMap.add 11 (Some 10)
    |> IntMap.add 12 (Some 11)
    |> IntMap.add 20 None
  in
  let group_info = {
    track_parent;
    group_ids = IntSet.(empty |> add 10 |> add 11);
    track_order = [10; 11; 12; 20];
  } in
  let output = Mermaid_renderer.render_nodes_with_groups
      ~direction:"LR"
      ~track_info_map
      ~main_node:{ id = "main"; label = "Main" }
      ~external_nodes:[]
      ~edges:[]
      ~group_info
  in
  let top_idx = index_of_substring ~haystack:output ~needle:"subgraph group_10[\"Top\"]" in
  let inner_idx = index_of_substring ~haystack:output ~needle:"subgraph group_11[\"Inner\"]" in
  check bool "nested order" true
    (match top_idx, inner_idx with
     | Some t, Some i -> t < i
     | _ -> false);
  check bool "ungrouped present" true
    (contains_substring ~haystack:output ~needle:"track_20[\"Solo (Audio)\"]")

let test_top_down_direction_header () =
  let open Flowchart in
  let group_info = {
    track_parent = IntMap.empty;
    group_ids = IntSet.empty;
    track_order = [];
  } in
  let output = Mermaid_renderer.render_nodes_with_groups
      ~direction:"TD"
      ~track_info_map:IntMap.empty
      ~main_node:{ id = "main"; label = "Main" }
      ~external_nodes:[]
      ~edges:[]
      ~group_info
  in
  check bool "direction header TD" true
    (contains_substring ~haystack:output ~needle:"flowchart TD")

let test_main_keyword_classifier () =
  let open Flowchart in
  check bool "main keyword exact" true (is_main_keyword "main");
  check bool "main keyword case-insensitive" true (is_main_keyword "Main");
  check bool "main keyword with spaces" true (is_main_keyword " main ");
  check bool "main keyword master" true (is_main_keyword "master");
  check bool "main keyword Master case" true (is_main_keyword "Master")

let test_group_parent_target_resolution () =
  let open Flowchart in
  let track_parent =
    IntMap.empty
    |> IntMap.add 74 None
    |> IntMap.add 75 (Some 74)
  in
  let track_id_map =
    IntMap.empty
    |> IntMap.add 74 "track_74"
    |> IntMap.add 75 "track_75"
  in
  let resolved = resolve_group_parent_node_id
      ~source_track_id:(Some 75)
      ~track_parent
      ~group_ids:(IntSet.singleton 74)
      ~track_id_map
  in
  check (option string) "group target resolves to parent group node"
    (Some "track_74") resolved

let test_group_parent_target_resolution_none_without_parent_group () =
  let open Flowchart in
  let track_parent =
    IntMap.empty
    |> IntMap.add 74 None
    |> IntMap.add 75 None
  in
  let track_id_map =
    IntMap.empty
    |> IntMap.add 74 "track_74"
    |> IntMap.add 75 "track_75"
  in
  let resolved = resolve_group_parent_node_id
      ~source_track_id:(Some 75)
      ~track_parent
      ~group_ids:(IntSet.singleton 74)
      ~track_id_map
  in
  check (option string) "no parent group means no resolution" None resolved

let test_parent_group_routing_edge_suppressed () =
  let open Flowchart in
  let track_parent =
    IntMap.empty
    |> IntMap.add 74 None
    |> IntMap.add 75 (Some 74)
  in
  let track_id_map =
    IntMap.empty
    |> IntMap.add 74 "track_74"
    |> IntMap.add 75 "track_75"
  in
  let suppressed = should_suppress_parent_group_routing_edge
      ~source_track_id:(Some 75)
      ~target_id:"track_74"
      ~track_parent
      ~group_ids:(IntSet.singleton 74)
      ~track_id_map
  in
  check bool "parent-group routing edge suppressed" true suppressed

let test_parent_group_routing_edge_not_suppressed_for_other_target () =
  let open Flowchart in
  let track_parent =
    IntMap.empty
    |> IntMap.add 74 None
    |> IntMap.add 75 (Some 74)
  in
  let track_id_map =
    IntMap.empty
    |> IntMap.add 42 "track_42"
    |> IntMap.add 74 "track_74"
    |> IntMap.add 75 "track_75"
  in
  let suppressed = should_suppress_parent_group_routing_edge
      ~source_track_id:(Some 75)
      ~target_id:"track_42"
      ~track_parent
      ~group_ids:(IntSet.singleton 74)
      ~track_id_map
  in
  check bool "non-parent target edge not suppressed" false suppressed

let test_main_node_hidden_when_unconnected () =
  let open Flowchart in
  let output = Mermaid_renderer.render_nodes_with_groups
      ~direction:"TD"
      ~track_info_map:IntMap.empty
      ~main_node:{ id = "main"; label = "Main" }
      ~external_nodes:[]
      ~edges:[]
      ~group_info:{ track_parent = IntMap.empty; group_ids = IntSet.empty; track_order = [] }
  in
  check bool "main hidden when no edges reference it" false
    (contains_substring ~haystack:output ~needle:"main[\"Main\"]")

let test_main_node_shown_when_connected () =
  let open Flowchart in
  let track_info_map =
    IntMap.empty
    |> IntMap.add 1 { node = { id = "track_1"; label = "Track 1 (Audio)" }; group_label = None }
  in
  let output = Mermaid_renderer.render_nodes_with_groups
      ~direction:"TD"
      ~track_info_map
      ~main_node:{ id = "main"; label = "Main" }
      ~external_nodes:[]
      ~edges:[{ from_id = "track_1"; to_id = "main"; label = "vol=1.00"; style = Routing }]
      ~group_info:{ track_parent = IntMap.empty; group_ids = IntSet.empty; track_order = [1] }
  in
  check bool "main shown when connected" true
    (contains_substring ~haystack:output ~needle:"main[\"Main\"]")

let test_routing_from_group_to_main_uses_subgraph_id () =
  let open Flowchart in
  let from_id = routing_from_id_for_main_target
      ~source_track_id:(Some 74)
      ~default_from_id:"track_74"
      ~target_id:"main"
      ~group_ids:(IntSet.singleton 74)
      ~main_node_id:"main"
      ~use_subgraph_id:true
  in
  check string "group source to main uses subgraph id" "group_74" from_id

let test_routing_from_group_to_non_main_keeps_track_node () =
  let open Flowchart in
  let from_id = routing_from_id_for_main_target
      ~source_track_id:(Some 74)
      ~default_from_id:"track_74"
      ~target_id:"track_42"
      ~group_ids:(IntSet.singleton 74)
      ~main_node_id:"main"
      ~use_subgraph_id:true
  in
  check string "group source to non-main keeps track node" "track_74" from_id

let test_routing_from_non_group_to_main_keeps_track_node () =
  let open Flowchart in
  let from_id = routing_from_id_for_main_target
      ~source_track_id:(Some 12)
      ~default_from_id:"track_12"
      ~target_id:"main"
      ~group_ids:(IntSet.singleton 74)
      ~main_node_id:"main"
      ~use_subgraph_id:true
  in
  check string "non-group source to main keeps track node" "track_12" from_id

let test_parse_target_track_id_from_audio_input_target () =
  let open Flowchart in
  let parsed = parse_target_track_id "AudioIn/Track.12/DeviceOut.1.S1" in
  check (option int) "parse Track.<id> from complex input target" (Some 12) parsed

let test_parse_target_track_id_no_false_s_suffix_parse () =
  let open Flowchart in
  let parsed = parse_target_track_id "AudioIn/External/S1" in
  check (option int) "do not parse trailing S1 as track id" None parsed

let test_external_input_node_created_for_audioin_external () =
  let open Flowchart in
  let open Alsdiff_base in
  let open Alsdiff_live in
  (* Create a minimal audio track with external input routing *)
  let audio_track = Track.Audio {
      id = 68;
      name = "Test Track";
      current_name = "Test Track";
      routings = {
        audio_in = {
          route_type = Track.Routing.AudioIn;
          target = "AudioIn/External/S1";
          upper_string = "Ext. In";
          lower_string = "1/2";
        };
        audio_out = {
          route_type = Track.Routing.AudioOut;
          target = "Main";
          upper_string = "Main";
          lower_string = "";
        };
        midi_in = {
          route_type = Track.Routing.MidiIn;
          target = "MidiIn/None";
          upper_string = "None";
          lower_string = "";
        };
        midi_out = {
          route_type = Track.Routing.MidiOut;
          target = "MidiOut/None";
          upper_string = "None";
          lower_string = "";
        };
      };
      mixer = {
        volume = { name = "Volume"; value = Device.Float 1.0; automation = 0; modulation = 0; mapping = None };
        pan = { name = "Pan"; value = Device.Float 0.0; automation = 0; modulation = 0; mapping = None };
        mute = { name = "Mute"; value = Device.Bool false; automation = 0; modulation = 0; mapping = None };
        solo = { name = "Solo"; value = Device.Bool false; automation = 0; modulation = 0; mapping = None };
        sends = [];
      };
      devices = [];
      clips = [];
      automations = [];
    } in
  let liveset = {
    Liveset.name = "Test";
    version = { major = "12"; minor = "0"; revision = "0" };
    creator = "Test";
    tracks = [audio_track];
    returns = [];
    main = Track.Main {
        name = "Main";
        current_name = "Main";
        routings = {
          audio_in = {
            route_type = Track.Routing.AudioIn;
            target = "AudioIn/None";
            upper_string = "None";
            lower_string = "";
          };
          audio_out = {
            route_type = Track.Routing.AudioOut;
            target = "AudioOut/None";
            upper_string = "None";
            lower_string = "";
          };
          midi_in = {
            route_type = Track.Routing.MidiIn;
            target = "MidiIn/None";
            upper_string = "None";
            lower_string = "";
          };
          midi_out = {
            route_type = Track.Routing.MidiOut;
            target = "MidiOut/None";
            upper_string = "None";
            lower_string = "";
          };
        };
        mixer = {
          base = {
            volume = { name = "Volume"; value = Device.Float 1.0; automation = 0; modulation = 0; mapping = None };
            pan = { name = "Pan"; value = Device.Float 0.0; automation = 0; modulation = 0; mapping = None };
            mute = { name = "Mute"; value = Device.Bool false; automation = 0; modulation = 0; mapping = None };
            solo = { name = "Solo"; value = Device.Bool false; automation = 0; modulation = 0; mapping = None };
            sends = [];
          };
          tempo = { name = "Tempo"; value = Device.Float 120.0; automation = 0; modulation = 0; mapping = None };
          time_signature = { name = "Time Signature"; value = Device.Int 4; automation = 0; modulation = 0; mapping = None };
          crossfade = { name = "Crossfade"; value = Device.Float 0.5; automation = 0; modulation = 0; mapping = None };
          global_groove = { name = "Global Groove"; value = Device.Float 1.0; automation = 0; modulation = 0; mapping = None };
        };
        devices = [];
        automations = [];
      };
    locators = [];
    pointees = Liveset.IntHashtbl.create 0;
  } in
  (* Create minimal XML structure with LiveSet and Tracks for build_group_info *)
  let xml = Xml.Element { name = "Ableton"; attrs = []; childs = [
      Xml.Element { name = "LiveSet"; attrs = []; childs = [
          Xml.Element { name = "Tracks"; attrs = []; childs = [
              Xml.Element { name = "AudioTrack"; attrs = [("Id", "68")]; childs = [
                  Xml.Element { name = "TrackGroupId"; attrs = [("Value", "-1")]; childs = [] }
                ] }
            ] }
        ] }
    ] } in
  let options = {
    direction = "LR";
    include_external = true;
    include_routing = true;
    include_sends = false;
    use_subgraph_id_for_groups = false;
  } in
  let _track_info_map, _main_node, external_nodes, edges, _group_info =
    build_graph ~xml ~liveset ~options
  in
  (* External input nodes are now created for AudioIn/External sources *)
  check bool "external input node created" true (List.length external_nodes > 0);
  let has_external_input_edge = List.exists (fun (e : edge) ->
      e.style = InputRouting && String.contains e.from_id '_'
    ) edges in
  check bool "external input edge created" true has_external_input_edge

let test_resolve_track_node_id_from_target () =
  let open Flowchart in
  let track_id_map =
    IntMap.empty
    |> IntMap.add 12 "track_12"
    |> IntMap.add 68 "track_68"
  in
  let resolved = resolve_track_node_id_from_target
      ~target:"AudioIn/Track.12/DeviceOut.1.S1"
      ~track_id_map
  in
  check (option string) "resolve input target to source track node id" (Some "track_12") resolved

let test_input_routing_edge_direction_and_style () =
  let open Flowchart in
  let edge = {
    from_id = "track_12";
    to_id = "track_68";
    label = "3/4-Opal";
    style = InputRouting;
  } in
  let rendered = Mermaid_renderer.render_edge edge in
  check bool "input edge points source->consumer" true
    (contains_substring ~haystack:rendered ~needle:"track_12");
  check bool "input edge points to consumer" true
    (contains_substring ~haystack:rendered ~needle:"track_68");
  check bool "input edge is dashed" true
    (contains_substring ~haystack:rendered ~needle:"-.->");
  check bool "input edge preserves route label" true
    (contains_substring ~haystack:rendered ~needle:"3/4-Opal")

let test_is_no_route_none_variants () =
  let open Flowchart in
  (* Output-side none variants *)
  check bool "None is no-route" true (is_no_route "None");
  check bool "MidiOut/None is no-route" true (is_no_route "MidiOut/None");
  check bool "AudioOut/None is no-route" true (is_no_route "AudioOut/None");
  (* Input-side none variants (fixes bogus external nodes) *)
  check bool "MidiIn/None is no-route" true (is_no_route "MidiIn/None");
  check bool "AudioIn/None is no-route" true (is_no_route "AudioIn/None");
  check bool "Main is not no-route" false (is_no_route "Main")

let test_should_skip_routing_for_no_output_destination () =
  let open Flowchart in
  let routing : Alsdiff_live.Track.Routing.t = {
    route_type = Alsdiff_live.Track.Routing.MidiOut;
    target = "MidiOut/None";
    upper_string = "None";
    lower_string = "";
  } in
  check bool "routing to no-output is skipped" true (should_skip_routing routing)

let () =
  run "flowchart groups" [
    ("groups", [
        test_case "group membership" `Quick test_group_membership;
        test_case "nested groups" `Quick test_nested_groups;
        test_case "top-down direction header" `Quick test_top_down_direction_header;
        test_case "main keyword classifier" `Quick test_main_keyword_classifier;
        test_case "group parent target resolution" `Quick test_group_parent_target_resolution;
        test_case "group parent target resolution none"
          `Quick test_group_parent_target_resolution_none_without_parent_group;
        test_case "parent-group routing edge suppressed" `Quick test_parent_group_routing_edge_suppressed;
        test_case "parent-group routing edge not suppressed"
          `Quick test_parent_group_routing_edge_not_suppressed_for_other_target;
        test_case "main node hidden when unconnected" `Quick test_main_node_hidden_when_unconnected;
        test_case "main node shown when connected" `Quick test_main_node_shown_when_connected;
        test_case "routing from group to main uses subgraph id"
          `Quick test_routing_from_group_to_main_uses_subgraph_id;
        test_case "routing from group to non-main keeps track node"
          `Quick test_routing_from_group_to_non_main_keeps_track_node;
        test_case "routing from non-group to main keeps track node"
          `Quick test_routing_from_non_group_to_main_keeps_track_node;
        test_case "parse target track id from audio input target"
          `Quick test_parse_target_track_id_from_audio_input_target;
        test_case "parse target track id no false suffix parse"
          `Quick test_parse_target_track_id_no_false_s_suffix_parse;
        test_case "external input node created for AudioIn/External"
          `Quick test_external_input_node_created_for_audioin_external;
        test_case "resolve track node id from target"
          `Quick test_resolve_track_node_id_from_target;
        test_case "input routing edge direction and style"
          `Quick test_input_routing_edge_direction_and_style;
        test_case "none variants classify as no-route" `Quick test_is_no_route_none_variants;
        test_case "routing to no-output is skipped"
          `Quick test_should_skip_routing_for_no_output_destination;
      ]);
  ]
