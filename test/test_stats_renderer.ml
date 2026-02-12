open Alsdiff_output.View_model
open Alsdiff_output.Stats_renderer

let mk_field name change =
  Field { name; change; domain_type = DTOther; oldval = None; newval = None }

let mk_item name change domain_type children =
  Item { name; change; domain_type; children }

let mk_collection name change domain_type items =
  Collection { name; change; domain_type; items }

let test_empty_no_changes () =
  let views = [mk_item "LiveSet" Unchanged DTLiveset []] in
  let output = render views in
  Alcotest.(check string) "no changes" "No changes." output

let test_single_added_track () =
  let views =
    [ mk_item "LiveSet" Modified DTLiveset
        [ mk_collection "Tracks" Modified DTTrack
            [ mk_item "Track 1" Added DTTrack [mk_field "Name" Added] ]
        ]
    ]
  in
  let output = render views in
  Alcotest.(check string) "single added track" "Tracks: 1 Added" output

let test_mixed_changes () =
  let views =
    [ mk_item "LiveSet" Modified DTLiveset
        [ mk_collection "Tracks" Modified DTTrack
            [ mk_item "Track 1" Added DTTrack
                [ mk_collection "Devices" Modified DTDevice
                    [ mk_item "Compressor" Added DTDevice [];
                      mk_item "EQ8" Removed DTDevice [];
                      mk_item "Reverb" Modified DTDevice [] ]
                ;
                  mk_collection "Clips" Modified DTClip
                    [ mk_item "Clip A" Modified DTClip
                        [ mk_collection "Notes" Modified DTNote
                            [ mk_item "C4" Added DTNote [];
                              mk_item "E4" Modified DTNote [];
                              mk_item "G4" Modified DTNote [] ]
                        ]
                    ]
                ]
            ;
              mk_item "Track 2" Modified DTTrack
                [ mk_collection "Clips" Modified DTClip
                    [ mk_item "Clip 1" Removed DTClip [] ]
                ]
            ;
              mk_item "Track 3" Removed DTTrack []
            ]
        ]
    ]
  in
  let output = render views in
  let lines = String.split_on_char '\n' output in
  Alcotest.(check int) "line count" 4 (List.length lines);
  Alcotest.(check string) "tracks line" "Tracks: 1 Added, 1 Removed, 1 Modified"
    (List.nth lines 0);
  Alcotest.(check string) "devices line" "Devices: 1 Added, 1 Removed, 1 Modified"
    (List.nth lines 1);
  Alcotest.(check string) "clips line" "Clips: 1 Removed, 1 Modified"
    (List.nth lines 2);
  Alcotest.(check string) "notes line" "Notes: 1 Added, 2 Modified"
    (List.nth lines 3)

let test_non_reportable_types_excluded () =
  let views =
    [ mk_item "LiveSet" Modified DTLiveset
        [ mk_item "Mixer" Modified DTMixer [mk_field "Volume" Modified];
          mk_item "Loop" Added DTLoop [];
          mk_item "Version" Modified DTVersion [];
          mk_item "SampleRef" Added DTSampleRef [];
        ]
    ]
  in
  let output = render views in
  Alcotest.(check string) "non-reportable excluded" "No changes." output

let test_unchanged_items_omitted () =
  let views =
    [ mk_item "LiveSet" Modified DTLiveset
        [ mk_collection "Tracks" Modified DTTrack
            [ mk_item "Track 1" Unchanged DTTrack [];
              mk_item "Track 2" Added DTTrack []
            ]
        ]
    ]
  in
  let output = render views in
  Alcotest.(check string) "unchanged omitted" "Tracks: 1 Added" output

let test_ordering () =
  let views =
    [ mk_item "LiveSet" Modified DTLiveset
        [ mk_item "Locator 1" Added DTLocator [];
          mk_collection "Tracks" Modified DTTrack
            [ mk_item "Track 1" Modified DTTrack
                [ mk_collection "Notes" Modified DTNote
                    [ mk_item "C4" Added DTNote [] ]
                ]
            ]
        ]
    ]
  in
  let output = render views in
  let lines = String.split_on_char '\n' output in
  Alcotest.(check int) "line count" 3 (List.length lines);
  Alcotest.(check string) "tracks first" "Tracks: 1 Modified" (List.nth lines 0);
  Alcotest.(check string) "notes second" "Notes: 1 Added" (List.nth lines 1);
  Alcotest.(check string) "locators third" "Locators: 1 Added" (List.nth lines 2)

let test_deeply_nested () =
  let views =
    [ mk_item "LiveSet" Modified DTLiveset
        [ mk_collection "Tracks" Modified DTTrack
            [ mk_item "Track 1" Modified DTTrack
                [ mk_collection "Devices" Modified DTDevice
                    [ mk_item "Group" Modified DTDevice
                        [ mk_collection "Devices" Modified DTDevice
                            [ mk_item "Inner" Added DTDevice [] ]
                        ]
                    ]
                ]
            ]
        ]
    ]
  in
  let output = render views in
  let lines = String.split_on_char '\n' output in
  Alcotest.(check int) "line count" 2 (List.length lines);
  Alcotest.(check string) "tracks" "Tracks: 1 Modified" (List.nth lines 0);
  Alcotest.(check string) "devices counted" "Devices: 1 Added, 1 Modified" (List.nth lines 1)

let test_zero_count_change_types_omitted () =
  let views =
    [ mk_item "LiveSet" Modified DTLiveset
        [ mk_collection "Tracks" Modified DTTrack
            [ mk_item "Track 1" Added DTTrack [];
              mk_item "Track 2" Added DTTrack []
            ]
        ]
    ]
  in
  let output = render views in
  Alcotest.(check string) "only added shown" "Tracks: 2 Added" output

let tests =
  [
    "empty no changes", `Quick, test_empty_no_changes;
    "single added track", `Quick, test_single_added_track;
    "mixed changes", `Quick, test_mixed_changes;
    "non-reportable types excluded", `Quick, test_non_reportable_types_excluded;
    "unchanged items omitted", `Quick, test_unchanged_items_omitted;
    "ordering follows fixed order", `Quick, test_ordering;
    "deeply nested counting", `Quick, test_deeply_nested;
    "zero count change types omitted", `Quick, test_zero_count_change_types_omitted;
  ]

let () = Alcotest.run "Stats_renderer" [ "stats", tests ]
