(* Comprehensive test coverage for detail_config options in text_renderer.
   Tests all customizable options except type_overrides which is already covered in test_text_renderer.ml *)

open Alsdiff_output.Text_renderer
open Alsdiff_output.Config
open Alsdiff_output.View_model

(* ==================== Helper Functions ==================== *)

(* Create test item with fields *)
let make_test_item ~name ~change ~domain_type ~fields =
  Item {
    name;
    change;
    domain_type;
    children = List.map (fun (n, o, v) ->
        Field {
          name = n;
          change;
          domain_type = DTOther;
          oldval = Option.map (fun x -> Fstring x) o;
          newval = Option.map (fun x -> Fstring x) v;
        }
      ) fields
  }

(* Create simple item without fields *)
let simple_item ?(domain_type = DTOther) name change =
  Item { name; change; domain_type; children = [] }

(* Create simple field item *)
let simple_field_item name change field_name old_val new_val =
  Item {
    name;
    change;
    domain_type = DTOther;
    children = [
      Field {
        name = field_name;
        change;
        domain_type = DTOther;
        oldval = Option.map (fun x -> Fstring x) old_val;
        newval = Option.map (fun x -> Fstring x) new_val;
      }
    ]
  }

(* Render to string helper *)
let render_view cfg view =
  render_to_string cfg view

(* Trim and normalize output for comparison *)
let normalize_output s =
  String.trim s

(* Helper to check if output contains a literal substring *)
let contains_string pattern text =
  Re.execp (Re.compile (Re.str pattern)) text

(* ==================== 1. Base Detail Level Tests ==================== *)

(* 1.1. Test Ignore level hides items *)
let test_ignore_level_hides_items () =
  let cfg = { full with added = Ignore } in
  let view = simple_field_item "TestItem" Added "Field" None (Some "value") in
  let output = render_view cfg view |> normalize_output in
  Alcotest.(check string) "ignore hides added items" "" output

(* 1.2. Test Summary level shows counts *)
let test_summary_level_shows_counts () =
  let cfg = { full with added = Summary } in
  let view = make_test_item ~name:"Item" ~change:Added ~domain_type:DTOther
      ~fields:[("Field1", None, Some "val1"); ("Field2", None, Some "val2")] in
  let output = render_view cfg view |> normalize_output in
  Alcotest.(check string) "summary shows counts" "+ Item (2 Added)" output

(* 1.3. Test Compact level name only *)
let test_compact_level_name_only () =
  let cfg = { full with added = Compact } in
  let view = make_test_item ~name:"Item" ~change:Added ~domain_type:DTOther
      ~fields:[("Field1", None, Some "val1")] in
  let output = render_view cfg view |> normalize_output in
  Alcotest.(check string) "compact name only" "+ Item" output

(* 1.4. Test Inline level shows fields inline *)
let test_inline_level_shows_fields_inline () =
  let cfg = { full with added = Inline } in
  let view = make_test_item ~name:"Item" ~change:Added ~domain_type:DTOther
      ~fields:[("Field1", None, Some "val1"); ("Field2", None, Some "val2")] in
  let output = render_view cfg view |> normalize_output in
  Alcotest.(check string) "inline shows fields" "+ Item [Field1: val1, Field2: val2]" output

(* 1.5. Test Full level shows fields multiline *)
let test_full_level_shows_fields_multiline () =
  let cfg = { full with added = Full } in
  let view = make_test_item ~name:"Item" ~change:Added ~domain_type:DTOther
      ~fields:[("Field1", None, Some "val1"); ("Field2", None, Some "val2")] in
  let output = render_view cfg view |> normalize_output in
  (* Full mode shows multiline with 2-space indent *)
  let expected = "+ Item\n  + Field1: val1\n  + Field2: val2" in
  Alcotest.(check string) "full multiline" expected output

(* 1.6. Test Unchanged Full visible *)
let test_unchanged_full_visible () =
  let cfg = { full with unchanged = Full } in
  let view = simple_item "Item" Unchanged in
  let output = render_view cfg view |> normalize_output in
  Alcotest.(check string) "unchanged full visible" "= Item" output

(* 1.7. Test Unchanged Ignore hidden *)
let test_unchanged_ignore_hidden () =
  let cfg = { full with unchanged = Ignore } in
  let view = simple_item "Item" Unchanged in
  let output = render_view cfg view |> normalize_output in
  Alcotest.(check string) "unchanged ignore hidden" "" output

(* ==================== 2. max_collection_items Tests ==================== *)

(* 2.1. Test max_collection_items None unlimited *)
let test_max_collection_items_none_unlimited () =
  let cfg = { full with max_collection_items = None } in
  let items = List.init 100 (fun _ -> simple_item "Item" Added) in
  let view = Collection { name = "Items"; change = Added; domain_type = DTOther; items } in
  let output = render_view cfg view in
  (* Count lines - should have 100 "Items" lines + 1 header = 101 lines *)
  let lines = String.split_on_char '\n' output |> List.filter (fun s -> String.trim s <> "") in
  Alcotest.(check int) "unlimited shows all" 101 (List.length lines)

(* 2.2. Test max_collection_items limits output *)
let test_max_collection_items_limits_output () =
  let cfg = { full with max_collection_items = Some 5 } in
  let items = List.init 20 (fun _ -> simple_item "Item" Added) in
  let view = Collection { name = "Items"; change = Added; domain_type = DTOther; items } in
  let output = render_view cfg view in
  let lines = String.split_on_char '\n' output |> List.filter (fun s -> String.trim s <> "") in
  (* 1 header + 5 items + 1 truncation = 7 lines *)
  Alcotest.(check int) "limited output" 7 (List.length lines)

(* 2.3. Test max_collection_items zero shows none *)
let test_max_collection_items_zero_shows_none () =
  let cfg = { full with max_collection_items = Some 0 } in
  let items = List.init 10 (fun _ -> simple_item "Item" Added) in
  let view = Collection { name = "Items"; change = Added; domain_type = DTOther; items } in
  let output = render_view cfg view |> normalize_output in
  (* With Full mode and max=0, no items are shown and output is empty.
     The collection filter limits to 0 items, leaving nothing to render. *)
  Alcotest.(check string) "zero shows nothing" "" output

(* 2.4. Test max_collection_items truncation breakdown *)
let test_max_collection_items_truncation_breakdown () =
  let cfg = { full with max_collection_items = Some 5 } in
  let added_items = List.init 10 (fun _ -> simple_item "Item" Added) in
  let removed_items = List.init 5 (fun _ -> simple_item "Item" Removed) in
  let all_items = added_items @ removed_items in
  let view = Collection { name = "Items"; change = Modified; domain_type = DTOther; items = all_items } in
  let output = render_view cfg view in
  (* Should show breakdown of truncated items *)
  Alcotest.(check bool) "contains added breakdown" true (contains_string "5 Added" output);
  Alcotest.(check bool) "contains removed breakdown" true (contains_string "5 Removed" output)

(* ==================== 3. Custom Prefix Tests ==================== *)

(* 3.1. Test custom prefix added *)
let test_custom_prefix_added () =
  let cfg = { full with prefix_added = "ADD" } in
  let view = simple_item "Item" Added in
  let output = render_view cfg view |> normalize_output in
  Alcotest.(check string) "custom added prefix" "ADD Item" output

(* 3.2. Test custom prefix removed *)
let test_custom_prefix_removed () =
  let cfg = { full with prefix_removed = "REM" } in
  let view = simple_item "Item" Removed in
  let output = render_view cfg view |> normalize_output in
  Alcotest.(check string) "custom removed prefix" "REM Item" output

(* 3.3. Test custom prefix modified *)
let test_custom_prefix_modified () =
  let cfg = { full with prefix_modified = "MOD" } in
  let view = simple_item "Item" Modified in
  let output = render_view cfg view |> normalize_output in
  Alcotest.(check string) "custom modified prefix" "MOD Item" output

(* 3.4. Test custom prefix unchanged *)
let test_custom_prefix_unchanged () =
  let cfg = { full with prefix_unchanged = "SAME"; unchanged = Full } in
  let view = simple_item "Item" Unchanged in
  let output = render_view cfg view |> normalize_output in
  Alcotest.(check string) "custom unchanged prefix" "SAME Item" output

(* 3.5. Test custom prefixes all together *)
let test_custom_prefixes_all_together () =
  let cfg = { full with prefix_added = "[+]"; prefix_removed = "[-]"; prefix_modified = "[*]" } in
  let items = [
    simple_item "AddedItem" Added;
    simple_item "RemovedItem" Removed;
    simple_item "ModifiedItem" Modified;
  ] in
  let view = Collection { name = "Items"; change = Modified; domain_type = DTOther; items } in
  let output = render_view cfg view in
  Alcotest.(check bool) "has added prefix" true (contains_string "[+]" output);
  Alcotest.(check bool) "has removed prefix" true (contains_string "[-]" output);
  Alcotest.(check bool) "has modified prefix" true (contains_string "[*]" output)

(* 3.6. Test with_prefixes helper function *)
let test_with_prefixes_helper_function () =
  let base_cfg = full in
  let cfg = with_prefixes ~added:"NEW" ~removed:"DEL" ~modified:"CHG" ~unchanged:"=" base_cfg in
  let items = [
    simple_item "AddedItem" Added;
    simple_item "RemovedItem" Removed;
    simple_item "ModifiedItem" Modified;
  ] in
  let view = Collection { name = "Items"; change = Modified; domain_type = DTOther; items } in
  let output = render_view cfg view in
  Alcotest.(check bool) "helper has NEW prefix" true (contains_string "NEW" output);
  Alcotest.(check bool) "helper has DEL prefix" true (contains_string "DEL" output);
  Alcotest.(check bool) "helper has CHG prefix" true (contains_string "CHG" output)

(* ==================== 4. note_name_style Tests ==================== *)

(* 4.1. Test note_name_style Sharp *)
let test_note_name_style_sharp () =
  (* MIDI note 54 = F#3 / Gb3 *)
  let note_name = get_note_name_from_int ~style:Sharp 54 in
  Alcotest.(check string) "sharp F#3" "F#3" note_name

(* 4.2. Test note_name_style Flat *)
let test_note_name_style_flat () =
  (* MIDI note 54 = F#3 / Gb3 *)
  let note_name = get_note_name_from_int ~style:Flat 54 in
  Alcotest.(check string) "flat Gb3" "Gb3" note_name

(* 4.3. Test note_name_style various notes *)
let test_note_name_style_various_notes () =
  (* Test multiple notes: 54 (F#/Gb), 56 (G#/Ab), 58 (A#/Bb) *)
  let sharp_54 = get_note_name_from_int ~style:Sharp 54 in
  let sharp_56 = get_note_name_from_int ~style:Sharp 56 in
  let sharp_58 = get_note_name_from_int ~style:Sharp 58 in
  Alcotest.(check string) "sharp 54" "F#3" sharp_54;
  Alcotest.(check string) "sharp 56" "G#3" sharp_56;
  Alcotest.(check string) "sharp 58" "A#3" sharp_58;

  let flat_54 = get_note_name_from_int ~style:Flat 54 in
  let flat_56 = get_note_name_from_int ~style:Flat 56 in
  let flat_58 = get_note_name_from_int ~style:Flat 58 in
  Alcotest.(check string) "flat 54" "Gb3" flat_54;
  Alcotest.(check string) "flat 56" "Ab3" flat_56;
  Alcotest.(check string) "flat 58" "Bb3" flat_58

(* ==================== 5. indent_width Tests ==================== *)

(* 5.1. Test indent_width default *)
let test_indent_width_default () =
  let cfg = { full with indent_width = 2 } in
  let view = make_test_item ~name:"Parent" ~change:Added ~domain_type:DTOther
      ~fields:[("Field", None, Some "value")] in
  let output = render_view cfg view in
  (* Should have 2 spaces before the field *)
  let lines = String.split_on_char '\n' output |> List.filter (fun s -> String.trim s <> "") in
  Alcotest.(check int) "has 2 lines" 2 (List.length lines);
  (* Second line should start with 2 spaces *)
  let second_line = List.nth lines 1 in
  Alcotest.(check bool) "indented with 2 spaces" true (String.starts_with ~prefix:"  " second_line)

(* 5.2. Test indent_width four spaces *)
let test_indent_width_four_spaces () =
  let cfg = { full with indent_width = 4 } in
  let view = make_test_item ~name:"Parent" ~change:Added ~domain_type:DTOther
      ~fields:[("Field", None, Some "value")] in
  let output = render_view cfg view in
  let lines = String.split_on_char '\n' output |> List.filter (fun s -> String.trim s <> "") in
  let second_line = List.nth lines 1 in
  Alcotest.(check bool) "indented with 4 spaces" true (String.starts_with ~prefix:"    " second_line)

(* 5.3. Test indent_width zero *)
let test_indent_width_zero () =
  let cfg = { full with indent_width = 0 } in
  let view = make_test_item ~name:"Parent" ~change:Added ~domain_type:DTOther
      ~fields:[("Field", None, Some "value")] in
  let output = render_view cfg view in
  let lines = String.split_on_char '\n' output |> List.filter (fun s -> String.trim s <> "") in
  let second_line = List.nth lines 1 in
  (* With indent_width=0, field line should not be indented *)
  Alcotest.(check bool) "no indentation" true
    (not (String.starts_with ~prefix:" " second_line))

(* 5.4. Test indent_width large *)
let test_indent_width_large () =
  let cfg = { full with indent_width = 8 } in
  let view = make_test_item ~name:"Parent" ~change:Added ~domain_type:DTOther
      ~fields:[("Field", None, Some "value")] in
  let output = render_view cfg view in
  let lines = String.split_on_char '\n' output |> List.filter (fun s -> String.trim s <> "") in
  let second_line = List.nth lines 1 in
  Alcotest.(check bool) "indented with 8 spaces" true (String.starts_with ~prefix:"        " second_line)

(* ==================== 6. Combination Tests ==================== *)

(* 6.1. Test Summary with custom prefix *)
let test_summary_with_custom_prefix () =
  let cfg = { full with added = Summary; prefix_added = ">>>" } in
  let view = make_test_item ~name:"Item" ~change:Added ~domain_type:DTOther
      ~fields:[("Field1", None, Some "val1"); ("Field2", None, Some "val2")] in
  let output = render_view cfg view |> normalize_output in
  Alcotest.(check string) "summary with custom prefix" ">>> Item (2 Added)" output

(* 6.2. Test Inline with custom indent *)
let test_inline_with_custom_indent () =
  let cfg = { full with added = Inline; indent_width = 4 } in
  (* Create nested structure: parent with field and nested item *)
  let child_item = simple_item "Child" Added in
  let view = Item {
      name = "Parent";
      change = Added;
      domain_type = DTOther;
      children = [
        Field { name = "Field"; change = Added; domain_type = DTOther; oldval = None; newval = Some (Fstring "value") };
        child_item;
      ]
    } in
  let output = render_view cfg view in
  let lines = String.split_on_char '\n' output |> List.filter (fun s -> String.trim s <> "") in
  (* Second line (nested item) should be indented with 4 spaces *)
  let second_line = List.nth lines 1 in
  Alcotest.(check bool) "nested with 4 space indent" true (String.starts_with ~prefix:"    " second_line)

(* 6.3. Test max_items with custom prefixes *)
let test_max_items_with_custom_prefixes () =
  let cfg = { full with max_collection_items = Some 2; prefix_added = "NEW"; prefix_removed = "DEL" } in
  let added_items = List.init 5 (fun _ -> simple_item "Item" Added) in
  let removed_items = List.init 3 (fun _ -> simple_item "Item" Removed) in
  let all_items = added_items @ removed_items in
  let view = Collection { name = "Items"; change = Modified; domain_type = DTOther; items = all_items } in
  let output = render_view cfg view in
  (* The collection shows items with custom prefixes for Added and Removed items *)
  Alcotest.(check bool) "has NEW prefix" true (contains_string "NEW" output);
  (* Note: truncation message uses words "Added"/"Removed", not custom prefixes *)
  Alcotest.(check bool) "has Added breakdown" true (contains_string "Added" output);
  Alcotest.(check bool) "has truncation" true (contains_string "more" output)

(* ==================== Additional Edge Cases ==================== *)

(* Test Modified with old/new values in Full mode *)
let test_modified_full_with_old_new () =
  let cfg = { full with modified = Full } in
  let view = make_test_item ~name:"Item" ~change:Modified ~domain_type:DTOther
      ~fields:[("Field", Some "old", Some "new")] in
  let output = render_view cfg view |> normalize_output in
  let expected = "* Item\n  * Field: old -> new" in
  Alcotest.(check string) "modified full old->new" expected output

(* Test Removed item shows old value only *)
let test_removed_shows_old_value () =
  let cfg = { full with removed = Full } in
  let view = make_test_item ~name:"Item" ~change:Removed ~domain_type:DTOther
      ~fields:[("Field", Some "old", None)] in
  let output = render_view cfg view |> normalize_output in
  let expected = "- Item\n  - Field: old" in
  Alcotest.(check string) "removed old value" expected output

(* Test Added item shows new value only *)
let test_added_shows_new_value () =
  let cfg = { full with added = Full } in
  let view = make_test_item ~name:"Item" ~change:Added ~domain_type:DTOther
      ~fields:[("Field", None, Some "new")] in
  let output = render_view cfg view |> normalize_output in
  let expected = "+ Item\n  + Field: new" in
  Alcotest.(check string) "added new value" expected output

(* Test Inline with Modified field *)
let test_inline_modified_field () =
  let cfg = { full with modified = Inline } in
  let view = make_test_item ~name:"Item" ~change:Modified ~domain_type:DTOther
      ~fields:[("Field", Some "old", Some "new")] in
  let output = render_view cfg view |> normalize_output in
  Alcotest.(check string) "inline modified" "* Item [Field: old -> new]" output

(* Test Compact collection in Summary mode *)
let test_compact_collection_summary () =
  let cfg = { full with added = Summary } in
  let items = List.init 10 (fun _ -> simple_item "Item" Added) in
  let view = Collection { name = "Items"; change = Added; domain_type = DTOther; items } in
  let output = render_view cfg view |> normalize_output in
  Alcotest.(check bool) "summary shows count" true (contains_string "(10 Added)" output)

(* Test Compact on a Collection renders header only, no items *)
let test_compact_collection_header_only () =
  let cfg = { full with added = Compact } in
  let items = List.init 5 (fun _ -> simple_item "Item" Added) in
  let view = Collection { name = "Items"; change = Added; domain_type = DTOther; items } in
  let output = render_view cfg view |> normalize_output in
  Alcotest.(check string) "compact collection header only" "+ Items" output

(* Test negative indent_width does not crash *)
let test_indent_width_negative () =
  let cfg = { full with indent_width = -2 } in
  let view = make_test_item ~name:"Parent" ~change:Added ~domain_type:DTOther
      ~fields:[("Field", None, Some "value")] in
  let _output = render_view cfg view in
  (* Negative indent should not crash — just verifying no exception *)
  Alcotest.(check pass) "negative indent doesn't crash" () ()

(* Test max_collection_items=0 with Summary mode still shows counts *)
let test_max_collection_items_zero_summary () =
  let cfg = { full with added = Summary; max_collection_items = Some 0 } in
  let items = List.init 10 (fun _ -> simple_item "Item" Added) in
  let view = Collection { name = "Items"; change = Added; domain_type = DTOther; items } in
  let output = render_view cfg view |> normalize_output in
  (* Summary mode shows counts regardless of max_collection_items *)
  Alcotest.(check string) "summary ignores max" "+ Items (10 Added)" output

(* ==================== Test Suite ==================== *)

let tests = [
  (* Base Detail Level Tests *)
  "ignore level hides items", `Quick, test_ignore_level_hides_items;
  "summary shows counts", `Quick, test_summary_level_shows_counts;
  "compact name only", `Quick, test_compact_level_name_only;
  "inline shows fields", `Quick, test_inline_level_shows_fields_inline;
  "full multiline", `Quick, test_full_level_shows_fields_multiline;
  "unchanged full visible", `Quick, test_unchanged_full_visible;
  "unchanged ignore hidden", `Quick, test_unchanged_ignore_hidden;

  (* max_collection_items Tests *)
  "max_items none unlimited", `Quick, test_max_collection_items_none_unlimited;
  "max_items limits output", `Quick, test_max_collection_items_limits_output;
  "max_items zero shows none", `Quick, test_max_collection_items_zero_shows_none;
  "max_items truncation breakdown", `Quick, test_max_collection_items_truncation_breakdown;

  (* Custom Prefix Tests *)
  "custom prefix added", `Quick, test_custom_prefix_added;
  "custom prefix removed", `Quick, test_custom_prefix_removed;
  "custom prefix modified", `Quick, test_custom_prefix_modified;
  "custom prefix unchanged", `Quick, test_custom_prefix_unchanged;
  "custom prefixes all together", `Quick, test_custom_prefixes_all_together;
  "with_prefixes helper", `Quick, test_with_prefixes_helper_function;

  (* note_name_style Tests *)
  "note style sharp", `Quick, test_note_name_style_sharp;
  "note style flat", `Quick, test_note_name_style_flat;
  "note style various notes", `Quick, test_note_name_style_various_notes;

  (* indent_width Tests *)
  "indent width default", `Quick, test_indent_width_default;
  "indent width four spaces", `Quick, test_indent_width_four_spaces;
  "indent width zero", `Quick, test_indent_width_zero;
  "indent width large", `Quick, test_indent_width_large;

  (* Combination Tests *)
  "summary with custom prefix", `Quick, test_summary_with_custom_prefix;
  "inline with custom indent", `Quick, test_inline_with_custom_indent;
  "max_items with custom prefixes", `Quick, test_max_items_with_custom_prefixes;

  (* Edge Cases *)
  "modified full old->new", `Quick, test_modified_full_with_old_new;
  "removed shows old value", `Quick, test_removed_shows_old_value;
  "added shows new value", `Quick, test_added_shows_new_value;
  "inline modified field", `Quick, test_inline_modified_field;
  "compact collection summary", `Quick, test_compact_collection_summary;

  (* New Coverage Tests *)
  "compact collection header only", `Quick, test_compact_collection_header_only;
  "indent width negative", `Quick, test_indent_width_negative;
  "max_items zero summary", `Quick, test_max_collection_items_zero_summary;
]

let () =
  Alcotest.run "ConfigTextRenderer" [
    "detail_config options", tests
  ]
