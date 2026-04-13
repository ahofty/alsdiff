(** Regex streaming tests ported from test_regex_match.ml.

    Uses [data/regex_match.xml] via [Xml2.stream_from_file]. Tests regex patterns,
    raw string matching, wildcard regex, attributes, escape preservation, and
    index-adapted regex queries. *)

open Alsdiff_base

let resolve_test_data_path filename =
  if Sys.file_exists ("data/" ^ filename) then "data/" ^ filename
  else if Sys.file_exists filename then filename
  else if Sys.file_exists ("test/data/" ^ filename) then "test/data/" ^ filename
  else if Sys.file_exists ("test/" ^ filename) then "test/" ^ filename
  else failwith (Printf.sprintf "Cannot find test data file: %s" filename)

let regex_xml_path = lazy (resolve_test_data_path "regex_match.xml")

let eval_query path_str : Upath2.match_result list =
  let q = Upath2.query_of_path ~qid:0 ~path_str ~attr:None in
  let nfa = Upath2.compile [ q ] in
  let stream = Xml2.stream_from_file (Lazy.force regex_xml_path) in
  Upath2.evaluate nfa stream

let check_count label expected (results : Upath2.match_result list) =
  Alcotest.(check int) label expected (List.length results)

let check_has_element label name (results : Upath2.match_result list) =
  Alcotest.(check bool) label true
    (List.exists (fun (r : Upath2.match_result) -> r.element_name = name) results)

(* --- Tests --- *)

let test_load_regex_match_xml () =
  let r = eval_query "/**/'MacroControls.*'" in
  check_count "macro controls count" 16 r

let test_regex_pattern_all_controls () =
  let r = eval_query "/**/'MacroControls\\.[0-9]+'" in
  check_count "all macro controls matched" 16 r;
  check_has_element "MacroControls.0 found" "MacroControls.0" r;
  check_has_element "MacroControls.15 found" "MacroControls.15" r

let test_regex_pattern_range_0_to_3 () =
  let r = eval_query "/**/'MacroControls\\.[0-3]$'" in
  check_count "macro controls 0-3 matched" 4 r;
  check_has_element "MacroControls.0 found" "MacroControls.0" r;
  check_has_element "MacroControls.3 found" "MacroControls.3" r

let test_regex_pattern_range_1_to_5 () =
  let r = eval_query "/**/'MacroControls\\.[1-5]$'" in
  check_count "macro controls 1-5 matched" 5 r;
  check_has_element "MacroControls.1 found" "MacroControls.1" r;
  check_has_element "MacroControls.5 found" "MacroControls.5" r

let test_regex_pattern_single_digit () =
  let r = eval_query "/**/'MacroControls\\.[0-9]$'" in
  check_count "single digit macro controls matched" 10 r;
  check_has_element "MacroControls.9 found" "MacroControls.9" r

let test_regex_pattern_double_digit () =
  let r = eval_query "/**/'MacroControls\\.[1][0-5]'" in
  check_count "double digit macro controls matched" 6 r;
  check_has_element "MacroControls.10 found" "MacroControls.10" r;
  check_has_element "MacroControls.15 found" "MacroControls.15" r

let test_raw_string_matching () =
  let r = eval_query "/**/MacroControls.0" in
  check_count "exact MacroControls.0 match" 1 r;
  let no = eval_query "/**/MacroControls.999" in
  check_count "non-existent exact match" 0 no

let test_regex_wildcard_pattern () =
  let r = eval_query "/**/'MacroControls\\..*'" in
  check_count "wildcard macro controls matched" 16 r

let test_regex_plus_pattern () =
  let r = eval_query "/**/'MacroControls\\.[0-9]+'" in
  check_count "plus quantifier macro controls matched" 16 r

let test_regex_with_attributes () =
  let r =
    eval_query "/**/'MacroControls\\.[0-3]$'/Manual@Value=0"
  in
  check_count "macro controls with Manual Value=0" 3 r

let test_regex_index_access () =
  (* Index[2] selects the 3rd match — MacroControls.2 *)
  let r = eval_query "/**/'MacroControls\\.[0-9]'[2]" in
  check_count "regex with index selects one match" 1 r;
  check_has_element "is MacroControls.2" "MacroControls.2" r

let test_regex_no_matches () =
  let r = eval_query "/**/'MacroControls\\.[A-Z]'" in
  check_count "no letter macro controls matched" 0 r;
  let no = eval_query "/**/'MacroControls\\.[9][9-9]'" in
  check_count "no 99+ matched" 0 no

let test_regex_escape_preservation () =
  let xml_str = "<Root><a.b/><axb/></Root>" in
  let q = Upath2.query_of_path ~qid:0 ~path_str:"/**/'a\\.b$'" ~attr:None in
  let nfa = Upath2.compile [ q ] in
  let stream = Xml2.stream_from_string xml_str in
  let escaped = Upath2.evaluate nfa stream in
  check_count "escaped dot regex matches one element" 1 escaped;
  check_has_element "matches a.b" "a.b" escaped;

  let q2 = Upath2.query_of_path ~qid:0 ~path_str:"/**/'a.b$'" ~attr:None in
  let nfa2 = Upath2.compile [ q2 ] in
  let stream2 = Xml2.stream_from_string xml_str in
  let unescaped = Upath2.evaluate nfa2 stream2 in
  check_count "unescaped dot regex matches both elements" 2 unescaped;
  check_has_element "matches a.b" "a.b" unescaped

let () =
  Alcotest.run "Upath2 Regex Streaming" [
    "regex_matching", [
      Alcotest.test_case "load regex match XML" `Quick test_load_regex_match_xml;
      Alcotest.test_case "regex pattern all controls" `Quick test_regex_pattern_all_controls;
      Alcotest.test_case "regex pattern range 0-3" `Quick test_regex_pattern_range_0_to_3;
      Alcotest.test_case "regex pattern range 1-5" `Quick test_regex_pattern_range_1_to_5;
      Alcotest.test_case "regex pattern single digit" `Quick test_regex_pattern_single_digit;
      Alcotest.test_case "regex pattern double digit" `Quick test_regex_pattern_double_digit;
      Alcotest.test_case "raw string matching" `Quick test_raw_string_matching;
      Alcotest.test_case "regex wildcard pattern" `Quick test_regex_wildcard_pattern;
      Alcotest.test_case "regex plus pattern" `Quick test_regex_plus_pattern;
      Alcotest.test_case "regex with attributes" `Quick test_regex_with_attributes;
      Alcotest.test_case "regex index access" `Quick test_regex_index_access;
      Alcotest.test_case "regex no matches" `Quick test_regex_no_matches;
      Alcotest.test_case "regex escape preservation" `Quick test_regex_escape_preservation;
    ];
  ]
