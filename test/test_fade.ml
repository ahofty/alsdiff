open Alcotest
open Alsdiff_base
open Alsdiff_live.Clip.Fade
open Utils

let test_create_basic () =
  let xml = Xml.read_file (resolve_test_data_path "fade.xml") in
  let fade = create xml in
  check (float 0.001) "fade_in_length" 0.0 fade.fade_in_length;
  check (float 0.001) "fade_out_length" 8.4680920641858144 fade.fade_out_length;
  check bool "is_initialized" true fade.is_initialized;
  check int "crossfade_state" 1 fade.crossfade_state;
  check (float 0.001) "fade_in_curve_skew" 0.0 fade.fade_in_curve_skew;
  check (float 0.001) "fade_in_curve_slope" 0.0 fade.fade_in_curve_slope;
  check (float 0.001) "fade_out_curve_skew" 0.0 fade.fade_out_curve_skew;
  check (float 0.001) "fade_out_curve_slope" 0.0 fade.fade_out_curve_slope;
  check bool "is_default_fade_in" false fade.is_default_fade_in;
  check bool "is_default_fade_out" false fade.is_default_fade_out

let test_create_with_values () =
  let xml = Xml.Element { name = "Fades"; attrs = []; childs = [
      Xml.Element { name = "FadeInLength"; attrs = ["Value", "2.5"]; childs = [Xml.Data ""] };
      Xml.Element { name = "FadeOutLength"; attrs = ["Value", "3.5"]; childs = [Xml.Data ""] };
      Xml.Element { name = "ClipFadesAreInitialized"; attrs = ["Value", "true"]; childs = [Xml.Data ""] };
      Xml.Element { name = "CrossfadeInState"; attrs = ["Value", "2"]; childs = [Xml.Data ""] };
      Xml.Element { name = "FadeInCurveSkew"; attrs = ["Value", "0.5"]; childs = [Xml.Data ""] };
      Xml.Element { name = "FadeInCurveSlope"; attrs = ["Value", "0.7"]; childs = [Xml.Data ""] };
      Xml.Element { name = "FadeOutCurveSkew"; attrs = ["Value", "0.3"]; childs = [Xml.Data ""] };
      Xml.Element { name = "FadeOutCurveSlope"; attrs = ["Value", "0.6"]; childs = [Xml.Data ""] };
      Xml.Element { name = "IsDefaultFadeIn"; attrs = ["Value", "true"]; childs = [Xml.Data ""] };
      Xml.Element { name = "IsDefaultFadeOut"; attrs = ["Value", "true"]; childs = [Xml.Data ""] };
    ]} in
  let fade = create xml in
  check (float 0.001) "fade_in_length" 2.5 fade.fade_in_length;
  check (float 0.001) "fade_out_length" 3.5 fade.fade_out_length;
  check bool "is_initialized" true fade.is_initialized;
  check int "crossfade_state" 2 fade.crossfade_state;
  check (float 0.001) "fade_in_curve_skew" 0.5 fade.fade_in_curve_skew;
  check (float 0.001) "fade_in_curve_slope" 0.7 fade.fade_in_curve_slope;
  check (float 0.001) "fade_out_curve_skew" 0.3 fade.fade_out_curve_skew;
  check (float 0.001) "fade_out_curve_slope" 0.6 fade.fade_out_curve_slope;
  check bool "is_default_fade_in" true fade.is_default_fade_in;
  check bool "is_default_fade_out" true fade.is_default_fade_out

let test_invalid_element_name () =
  let xml = Xml.Element { name = "InvalidFades"; attrs = []; childs = [] } in
  try
    let _ = create xml in
    fail "Expected Xml_error for invalid element name"
  with
  | Xml.Xml_error (_, msg) ->
    check string "error message" "Invalid XML element for creating Fade" msg

let test_missing_fade_in_length () =
  let xml = Xml.Element { name = "Fades"; attrs = []; childs = [
      Xml.Element { name = "FadeOutLength"; attrs = ["Value", "3.5"]; childs = [Xml.Data ""] };
      Xml.Element { name = "ClipFadesAreInitialized"; attrs = ["Value", "true"]; childs = [Xml.Data ""] };
    ]} in
  try
    let _ = create xml in
    fail "Expected error for missing FadeInLength"
  with
  | Upath.Path_not_found _ -> () (* Expected - missing required attribute *)

let test_diff_fade_in_length_change () =
  let old_fade = { fade_in_length = 0.0; fade_out_length = 8.0; is_initialized = true; crossfade_state = 1; fade_in_curve_skew = 0.0; fade_in_curve_slope = 0.0; fade_out_curve_skew = 0.0; fade_out_curve_slope = 0.0; is_default_fade_in = false; is_default_fade_out = false } in
  let new_fade = { fade_in_length = 2.5; fade_out_length = 8.0; is_initialized = true; crossfade_state = 1; fade_in_curve_skew = 0.0; fade_in_curve_slope = 0.0; fade_out_curve_skew = 0.0; fade_out_curve_slope = 0.0; is_default_fade_in = false; is_default_fade_out = false } in
  let patch = diff old_fade new_fade in
  (match patch.fade_in_length with
   | `Modified m ->
     check (float 0.001) "old fade_in_length" 0.0 m.oldval;
     check (float 0.001) "new fade_in_length" 2.5 m.newval
   | _ -> fail "Expected fade_in_length to be modified")

let test_diff_fade_out_length_change () =
  let old_fade = { fade_in_length = 0.0; fade_out_length = 8.0; is_initialized = true; crossfade_state = 1; fade_in_curve_skew = 0.0; fade_in_curve_slope = 0.0; fade_out_curve_skew = 0.0; fade_out_curve_slope = 0.0; is_default_fade_in = false; is_default_fade_out = false } in
  let new_fade = { fade_in_length = 0.0; fade_out_length = 4.0; is_initialized = true; crossfade_state = 1; fade_in_curve_skew = 0.0; fade_in_curve_slope = 0.0; fade_out_curve_skew = 0.0; fade_out_curve_slope = 0.0; is_default_fade_in = false; is_default_fade_out = false } in
  let patch = diff old_fade new_fade in
  (match patch.fade_out_length with
   | `Modified m ->
     check (float 0.001) "old fade_out_length" 8.0 m.oldval;
     check (float 0.001) "new fade_out_length" 4.0 m.newval
   | _ -> fail "Expected fade_out_length to be modified")

let test_diff_is_initialized_change () =
  let old_fade = { fade_in_length = 0.0; fade_out_length = 8.0; is_initialized = false; crossfade_state = 1; fade_in_curve_skew = 0.0; fade_in_curve_slope = 0.0; fade_out_curve_skew = 0.0; fade_out_curve_slope = 0.0; is_default_fade_in = false; is_default_fade_out = false } in
  let new_fade = { fade_in_length = 0.0; fade_out_length = 8.0; is_initialized = true; crossfade_state = 1; fade_in_curve_skew = 0.0; fade_in_curve_slope = 0.0; fade_out_curve_skew = 0.0; fade_out_curve_slope = 0.0; is_default_fade_in = false; is_default_fade_out = false } in
  let patch = diff old_fade new_fade in
  (match patch.is_initialized with
   | `Modified m ->
     check bool "old is_initialized" false m.oldval;
     check bool "new is_initialized" true m.newval
   | _ -> fail "Expected is_initialized to be modified")

let test_diff_crossfade_state_change () =
  let old_fade = { fade_in_length = 0.0; fade_out_length = 8.0; is_initialized = true; crossfade_state = 0; fade_in_curve_skew = 0.0; fade_in_curve_slope = 0.0; fade_out_curve_skew = 0.0; fade_out_curve_slope = 0.0; is_default_fade_in = false; is_default_fade_out = false } in
  let new_fade = { fade_in_length = 0.0; fade_out_length = 8.0; is_initialized = true; crossfade_state = 1; fade_in_curve_skew = 0.0; fade_in_curve_slope = 0.0; fade_out_curve_skew = 0.0; fade_out_curve_slope = 0.0; is_default_fade_in = false; is_default_fade_out = false } in
  let patch = diff old_fade new_fade in
  (match patch.crossfade_state with
   | `Modified m ->
     check int "old crossfade_state" 0 m.oldval;
     check int "new crossfade_state" 1 m.newval
   | _ -> fail "Expected crossfade_state to be modified")

let test_diff_fade_in_curve_skew_change () =
  let old_fade = { fade_in_length = 0.0; fade_out_length = 8.0; is_initialized = true; crossfade_state = 1; fade_in_curve_skew = 0.0; fade_in_curve_slope = 0.0; fade_out_curve_skew = 0.0; fade_out_curve_slope = 0.0; is_default_fade_in = false; is_default_fade_out = false } in
  let new_fade = { fade_in_length = 0.0; fade_out_length = 8.0; is_initialized = true; crossfade_state = 1; fade_in_curve_skew = 0.5; fade_in_curve_slope = 0.0; fade_out_curve_skew = 0.0; fade_out_curve_slope = 0.0; is_default_fade_in = false; is_default_fade_out = false } in
  let patch = diff old_fade new_fade in
  (match patch.fade_in_curve_skew with
   | `Modified m ->
     check (float 0.001) "old fade_in_curve_skew" 0.0 m.oldval;
     check (float 0.001) "new fade_in_curve_skew" 0.5 m.newval
   | _ -> fail "Expected fade_in_curve_skew to be modified")

let test_diff_fade_in_curve_slope_change () =
  let old_fade = { fade_in_length = 0.0; fade_out_length = 8.0; is_initialized = true; crossfade_state = 1; fade_in_curve_skew = 0.0; fade_in_curve_slope = 0.0; fade_out_curve_skew = 0.0; fade_out_curve_slope = 0.0; is_default_fade_in = false; is_default_fade_out = false } in
  let new_fade = { fade_in_length = 0.0; fade_out_length = 8.0; is_initialized = true; crossfade_state = 1; fade_in_curve_skew = 0.0; fade_in_curve_slope = 0.7; fade_out_curve_skew = 0.0; fade_out_curve_slope = 0.0; is_default_fade_in = false; is_default_fade_out = false } in
  let patch = diff old_fade new_fade in
  (match patch.fade_in_curve_slope with
   | `Modified m ->
     check (float 0.001) "old fade_in_curve_slope" 0.0 m.oldval;
     check (float 0.001) "new fade_in_curve_slope" 0.7 m.newval
   | _ -> fail "Expected fade_in_curve_slope to be modified")

let test_diff_fade_out_curve_skew_change () =
  let old_fade = { fade_in_length = 0.0; fade_out_length = 8.0; is_initialized = true; crossfade_state = 1; fade_in_curve_skew = 0.0; fade_in_curve_slope = 0.0; fade_out_curve_skew = 0.0; fade_out_curve_slope = 0.0; is_default_fade_in = false; is_default_fade_out = false } in
  let new_fade = { fade_in_length = 0.0; fade_out_length = 8.0; is_initialized = true; crossfade_state = 1; fade_in_curve_skew = 0.0; fade_in_curve_slope = 0.0; fade_out_curve_skew = 0.3; fade_out_curve_slope = 0.0; is_default_fade_in = false; is_default_fade_out = false } in
  let patch = diff old_fade new_fade in
  (match patch.fade_out_curve_skew with
   | `Modified m ->
     check (float 0.001) "old fade_out_curve_skew" 0.0 m.oldval;
     check (float 0.001) "new fade_out_curve_skew" 0.3 m.newval
   | _ -> fail "Expected fade_out_curve_skew to be modified")

let test_diff_fade_out_curve_slope_change () =
  let old_fade = { fade_in_length = 0.0; fade_out_length = 8.0; is_initialized = true; crossfade_state = 1; fade_in_curve_skew = 0.0; fade_in_curve_slope = 0.0; fade_out_curve_skew = 0.0; fade_out_curve_slope = 0.0; is_default_fade_in = false; is_default_fade_out = false } in
  let new_fade = { fade_in_length = 0.0; fade_out_length = 8.0; is_initialized = true; crossfade_state = 1; fade_in_curve_skew = 0.0; fade_in_curve_slope = 0.0; fade_out_curve_skew = 0.0; fade_out_curve_slope = 0.6; is_default_fade_in = false; is_default_fade_out = false } in
  let patch = diff old_fade new_fade in
  (match patch.fade_out_curve_slope with
   | `Modified m ->
     check (float 0.001) "old fade_out_curve_slope" 0.0 m.oldval;
     check (float 0.001) "new fade_out_curve_slope" 0.6 m.newval
   | _ -> fail "Expected fade_out_curve_slope to be modified")

let test_diff_is_default_fade_in_change () =
  let old_fade = { fade_in_length = 0.0; fade_out_length = 8.0; is_initialized = true; crossfade_state = 1; fade_in_curve_skew = 0.0; fade_in_curve_slope = 0.0; fade_out_curve_skew = 0.0; fade_out_curve_slope = 0.0; is_default_fade_in = false; is_default_fade_out = false } in
  let new_fade = { fade_in_length = 0.0; fade_out_length = 8.0; is_initialized = true; crossfade_state = 1; fade_in_curve_skew = 0.0; fade_in_curve_slope = 0.0; fade_out_curve_skew = 0.0; fade_out_curve_slope = 0.0; is_default_fade_in = true; is_default_fade_out = false } in
  let patch = diff old_fade new_fade in
  (match patch.is_default_fade_in with
   | `Modified m ->
     check bool "old is_default_fade_in" false m.oldval;
     check bool "new is_default_fade_in" true m.newval
   | _ -> fail "Expected is_default_fade_in to be modified")

let test_diff_is_default_fade_out_change () =
  let old_fade = { fade_in_length = 0.0; fade_out_length = 8.0; is_initialized = true; crossfade_state = 1; fade_in_curve_skew = 0.0; fade_in_curve_slope = 0.0; fade_out_curve_skew = 0.0; fade_out_curve_slope = 0.0; is_default_fade_in = false; is_default_fade_out = false } in
  let new_fade = { fade_in_length = 0.0; fade_out_length = 8.0; is_initialized = true; crossfade_state = 1; fade_in_curve_skew = 0.0; fade_in_curve_slope = 0.0; fade_out_curve_skew = 0.0; fade_out_curve_slope = 0.0; is_default_fade_in = false; is_default_fade_out = true } in
  let patch = diff old_fade new_fade in
  (match patch.is_default_fade_out with
   | `Modified m ->
     check bool "old is_default_fade_out" false m.oldval;
     check bool "new is_default_fade_out" true m.newval
   | _ -> fail "Expected is_default_fade_out to be modified")

let test_diff_unchanged () =
  let fade = { fade_in_length = 0.0; fade_out_length = 8.0; is_initialized = true; crossfade_state = 1; fade_in_curve_skew = 0.0; fade_in_curve_slope = 0.0; fade_out_curve_skew = 0.0; fade_out_curve_slope = 0.0; is_default_fade_in = false; is_default_fade_out = false } in
  let patch = diff fade fade in
  check bool "patch is empty" true (Patch.is_empty patch)

let test_patch_is_empty_true () =
  let fade = { fade_in_length = 0.0; fade_out_length = 8.0; is_initialized = true; crossfade_state = 1; fade_in_curve_skew = 0.0; fade_in_curve_slope = 0.0; fade_out_curve_skew = 0.0; fade_out_curve_slope = 0.0; is_default_fade_in = false; is_default_fade_out = false } in
  let patch = diff fade fade in
  check bool "patch is empty" true (Patch.is_empty patch)

let test_patch_is_empty_false () =
  let old_fade = { fade_in_length = 0.0; fade_out_length = 8.0; is_initialized = true; crossfade_state = 1; fade_in_curve_skew = 0.0; fade_in_curve_slope = 0.0; fade_out_curve_skew = 0.0; fade_out_curve_slope = 0.0; is_default_fade_in = false; is_default_fade_out = false } in
  let new_fade = { fade_in_length = 2.5; fade_out_length = 8.0; is_initialized = true; crossfade_state = 1; fade_in_curve_skew = 0.0; fade_in_curve_slope = 0.0; fade_out_curve_skew = 0.0; fade_out_curve_slope = 0.0; is_default_fade_in = false; is_default_fade_out = false } in
  let patch = diff old_fade new_fade in
  check bool "patch is not empty" false (Patch.is_empty patch)

let () =
  run "Fade" [
    "parsing", [
      test_case "create basic fade" `Quick test_create_basic;
      test_case "create fade with values" `Quick test_create_with_values;
      test_case "invalid element name raises error" `Quick test_invalid_element_name;
      test_case "missing FadeInLength raises error" `Quick test_missing_fade_in_length;
    ];
    "diffing", [
      test_case "detect fade_in_length changes" `Quick test_diff_fade_in_length_change;
      test_case "detect fade_out_length changes" `Quick test_diff_fade_out_length_change;
      test_case "detect is_initialized changes" `Quick test_diff_is_initialized_change;
      test_case "detect crossfade_state changes" `Quick test_diff_crossfade_state_change;
      test_case "detect fade_in_curve_skew changes" `Quick test_diff_fade_in_curve_skew_change;
      test_case "detect fade_in_curve_slope changes" `Quick test_diff_fade_in_curve_slope_change;
      test_case "detect fade_out_curve_skew changes" `Quick test_diff_fade_out_curve_skew_change;
      test_case "detect fade_out_curve_slope changes" `Quick test_diff_fade_out_curve_slope_change;
      test_case "detect is_default_fade_in changes" `Quick test_diff_is_default_fade_in_change;
      test_case "detect is_default_fade_out changes" `Quick test_diff_is_default_fade_out_change;
      test_case "unchanged fade produces empty patch" `Quick test_diff_unchanged;
    ];
    "patch", [
      test_case "patch is empty for unchanged values" `Quick test_patch_is_empty_true;
      test_case "patch is not empty for changed values" `Quick test_patch_is_empty_false;
    ];
  ]
