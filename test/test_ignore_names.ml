open Alsdiff_output.Text_renderer
open Alsdiff_output.View_model

(* ==================== name_matches ==================== *)

let test_name_matches_leading_token () =
  (* Device names render as "<type> (#<id>): <display>"; the leading token matches. *)
  Alcotest.(check bool) "DJMFilter matches leading token" true
    (name_matches "DJMFilter" "DJMFilter (#3): DJMFilter");
  Alcotest.(check bool) "InstrumentGroupDevice matches" true
    (name_matches "InstrumentGroupDevice" "InstrumentGroupDevice (#0): InstrumentGroupDevice")

let test_name_matches_other_device_no_match () =
  Alcotest.(check bool) "kHs Gain does not match DJMFilter" false
    (name_matches "DJMFilter" "kHs Gain (#2): kHs Gain")

let test_name_matches_trailing_display () =
  (* For tracks the leading token is the type; the user name is the trailing segment. *)
  Alcotest.(check bool) "1 BEAT matches trailing display" true
    (name_matches "1 BEAT" "MidiTrack (#12): 1 BEAT");
  Alcotest.(check bool) "MidiTrack matches leading type" true
    (name_matches "MidiTrack" "MidiTrack (#12): 1 BEAT")

let test_name_matches_whole_name () =
  (* Names with no delimiter (e.g. MainTrack "Main") match the whole string. *)
  Alcotest.(check bool) "Main matches whole name" true (name_matches "Main" "Main");
  Alcotest.(check bool) "Device matches whole name" true (name_matches "Device" "Device")

let test_name_matches_no_substring () =
  (* Matching is exact per segment, not substring-anywhere. *)
  Alcotest.(check bool) "Filter is not a substring match" false
    (name_matches "Filter" "DJMFilter (#3): DJMFilter")

(* ==================== apply_ignore_names ==================== *)

let device name =
  Item {
    name;
    change = Modified;
    domain_type = DTDevice;
    children = [
      Field { name = "knob"; change = Modified; domain_type = DTParam;
              oldval = Some (Ffloat 0.5); newval = Some (Ffloat 0.2) };
    ];
  }

(* A track with a "Devices" collection holding three devices. *)
let sample_views () = [
  Item {
    name = "MidiTrack (#12): 1 BEAT";
    change = Modified;
    domain_type = DTTrack;
    children = [
      Collection {
        name = "Devices";
        change = Modified;
        domain_type = DTDevice;
        items = [
          device "DJMFilter (#3): DJMFilter";
          device "kHs Gain (#2): kHs Gain";
          device "InstrumentGroupDevice (#0): InstrumentGroupDevice";
        ];
      };
    ];
  };
]

(* Collect all Item names anywhere in a view list. *)
let rec collect_names (views : view list) : string list =
  List.concat_map (fun v ->
      match v with
      | Field _ -> []
      | Item it -> it.name :: collect_names it.children
      | Collection c -> c.name :: collect_names c.items)
    views

let cfg_with rules = { full with ignore_names = rules }

let test_apply_removes_matching_device () =
  let views = apply_ignore_names
      (cfg_with [ { domain_type = DTDevice; name = "DJMFilter" } ])
      (sample_views ())
  in
  let names = collect_names views in
  Alcotest.(check bool) "DJMFilter removed" false
    (List.mem "DJMFilter (#3): DJMFilter" names);
  Alcotest.(check bool) "kHs Gain kept" true
    (List.mem "kHs Gain (#2): kHs Gain" names);
  Alcotest.(check bool) "InstrumentGroupDevice kept" true
    (List.mem "InstrumentGroupDevice (#0): InstrumentGroupDevice" names);
  Alcotest.(check bool) "track kept" true
    (List.mem "MidiTrack (#12): 1 BEAT" names)

let test_apply_empty_rules_noop () =
  let views = apply_ignore_names (cfg_with []) (sample_views ()) in
  Alcotest.(check int) "no rules keeps all 3 devices" 3
    (List.length (List.filter (fun n -> n <> "Devices" && n <> "MidiTrack (#12): 1 BEAT")
                    (collect_names views)))

let test_apply_respects_domain_type () =
  (* A rule for the wrong domain_type must not remove the device. *)
  let views = apply_ignore_names
      (cfg_with [ { domain_type = DTClip; name = "DJMFilter" } ])
      (sample_views ())
  in
  Alcotest.(check bool) "DJMFilter kept under wrong domain_type" true
    (List.mem "DJMFilter (#3): DJMFilter" (collect_names views))

let test_apply_removes_whole_collection () =
  (* Matching the collection name + domain_type drops the whole collection. *)
  let views = apply_ignore_names
      (cfg_with [ { domain_type = DTDevice; name = "Devices" } ])
      (sample_views ())
  in
  Alcotest.(check bool) "Devices collection removed" false
    (List.mem "Devices" (collect_names views))

(* Simple substring check, avoiding any extra library dependency. *)
let contains ~needle haystack =
  let nl = String.length needle and hl = String.length haystack in
  let rec aux i = i + nl <= hl && (String.sub haystack i nl = needle || aux (i + 1)) in
  nl = 0 || aux 0

let test_render_drops_emptied_container () =
  (* Removing every device leaves an empty "Devices" collection; the renderer skips it. *)
  let cfg = cfg_with [
      { domain_type = DTDevice; name = "DJMFilter" };
      { domain_type = DTDevice; name = "kHs Gain" };
      { domain_type = DTDevice; name = "InstrumentGroupDevice" };
    ] in
  let output = render cfg (sample_views ()) in
  Alcotest.(check bool) "no Devices header in output" false
    (contains ~needle:"Devices" output);
  Alcotest.(check bool) "track header still present" true
    (contains ~needle:"1 BEAT" output)

let tests = [
  "name_matches leading token", `Quick, test_name_matches_leading_token;
  "name_matches other device no match", `Quick, test_name_matches_other_device_no_match;
  "name_matches trailing display", `Quick, test_name_matches_trailing_display;
  "name_matches whole name", `Quick, test_name_matches_whole_name;
  "name_matches no substring", `Quick, test_name_matches_no_substring;
  "apply removes matching device", `Quick, test_apply_removes_matching_device;
  "apply empty rules noop", `Quick, test_apply_empty_rules_noop;
  "apply respects domain_type", `Quick, test_apply_respects_domain_type;
  "apply removes whole collection", `Quick, test_apply_removes_whole_collection;
  "render drops emptied container", `Quick, test_render_drops_emptied_container;
]

let () =
  Alcotest.run "ignore_names tests" [
    "ignore_names", tests
  ]
