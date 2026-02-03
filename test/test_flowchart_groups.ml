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
  let output = render_nodes_with_groups
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
  let output = render_nodes_with_groups
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

let () =
  run "flowchart groups" [
    ("groups", [
        test_case "group membership" `Quick test_group_membership;
        test_case "nested groups" `Quick test_nested_groups;
      ]);
  ]
