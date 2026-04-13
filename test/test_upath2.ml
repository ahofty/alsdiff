(** Comprehensive streaming NFA tests ported from test_upath.ml, test_wildcard.ml,
    test_complex.ml.

    Uses [Xml2.stream_from_string] with inline XML constants to avoid DOM dependency.
    Index filtering and SingleWildcard depth semantics match DOM-based Upath. *)

open Alsdiff_base

(* --- XML string constants --- *)

let sample_xml_str =
  "<root>\
   <a id=\"1\">\
   <b>hello</b>\
   <c val=\"test\"/>\
   </a>\
   <a id=\"2\">\
   <d/>\
   <b lang=\"en\">world</b>\
   </a>\
   <e>\
   <child id=\"e-child\"/>\
   <f>\
   <b/>\
   </f>\
   </e>\
   <special type=\"magic\">\
   <child id=\"s-child\"/>\
   </special>\
   <child type=\"magic\"/>\
   </root>"

let sample_xml_nested_str =
  "<root>\
   <a id=\"1\">\
   <b>\
   <c v=\"1\"/>\
   <c v=\"2\"/>\
   </b>\
   </a>\
   <a id=\"2\">\
   <d>\
   <b>\
   <c v=\"3\"/>\
   </b>\
   </d>\
   </a>\
   </root>"

let wildcard_xml_str =
  "<root>\
   <parent1>\
   <child>direct_child</child>\
   </parent1>\
   <parent2>\
   <intermediate>\
   <child>deep_child</child>\
   </intermediate>\
   </parent2>\
   </root>"

let complex_xml_str =
  "<archive>\
   <library name=\"music\">\
   <section id=\"A\">\
   <artist name=\"Artist1\">\
   <album title=\"Album1.1\" year=\"2020\">\
   <track no=\"1\">Track1</track>\
   <track no=\"2\" feat=\"Artist2\">Track2</track>\
   </album>\
   <album title=\"Album1.2\" year=\"2022\">\
   <track no=\"1\">TrackA</track>\
   </album>\
   </artist>\
   </section>\
   <section id=\"B\">\
   <artist name=\"Artist2\">\
   <album title=\"Album2.1\" year=\"2021\">\
   <track no=\"1\">Single</track>\
   </album>\
   </artist>\
   <genre name=\"electronic\">\
   <artist name=\"Artist3\">\
   <album title=\"Album3.1\" year=\"2023\">\
   <track no=\"1\">E-Track1</track>\
   <track no=\"2\">E-Track2</track>\
   </album>\
   </artist>\
   </genre>\
   </section>\
   </library>\
   <library name=\"pictures\">\
   <album title=\"Holidays\" year=\"2022\">\
   <photo year=\"2022\" location=\"Beach\"/>\
   <photo year=\"2023\" location=\"Mountain\"/>\
   </album>\
   </library>\
   </archive>"

(* --- Test helpers --- *)

type results = Upath2.match_result list

let eval_query path_str xml_str : results =
  let q = Upath2.query_of_path ~qid:0 ~path_str ~attr:None in
  let nfa = Upath2.compile [ q ] in
  let stream = Xml2.stream_from_string xml_str in
  Upath2.evaluate nfa stream

let check_count label expected (results : results) =
  Alcotest.(check int) label expected (List.length results)

let check_empty label (results : results) =
  Alcotest.(check int) label 0 (List.length results)

let check_has_element label name (results : results) =
  Alcotest.(check bool) label true
    (List.exists (fun (r : Upath2.match_result) -> r.element_name = name) results)

let check_has_attr_value label attr_name expected (results : results) =
  Alcotest.(check bool) label true
    (List.exists (fun (r : Upath2.match_result) ->
         Upath2.get_attr r attr_name = Some expected) results)

let check_only_attr_value label attr_name expected (results : results) =
  Alcotest.(check int) label 1 (List.length results);
  Alcotest.(check string) label expected
    (Option.get (Upath2.get_attr (List.hd results) attr_name))

(* --- Section: find_path (from test_upath.ml) --- *)

let find_path_tests =
  let module T = struct
    let t01 () =
      (* Both a elements have b children — streaming returns all matches *)
      let r = eval_query "/root/a/b" sample_xml_str in
      check_count "/root/a/b" 2 r;
      check_has_element "name is b" "b" r

    let t02 () =
      (* Index[0] selects first a's b children *)
      let r = eval_query "/root/a[0]/b" sample_xml_str in
      check_count "/root/a[0]/b" 1 r;
      check_has_element "name is b" "b" r

    let t03 () =
      (* Index[1] selects second a's b children *)
      let r = eval_query "/root/a[1]/b" sample_xml_str in
      check_count "/root/a[1]/b" 1 r;
      check_has_attr_value "lang=en" "lang" "en" r

    let t04 () =
      let r = eval_query "/root/a@id=\"1\"/c" sample_xml_str in
      check_count "/root/a@id=\"1\"/c" 1 r;
      check_has_attr_value "val=test" "val" "test" r

    let t05 () =
      let r = eval_query "/root/a@id=\"2\"/d" sample_xml_str in
      check_count "/root/a@id=\"2\"/d" 1 r;
      check_has_element "name is d" "d" r

    let t06 () =
      let r = eval_query "/root/a@id=\"3\"/d" sample_xml_str in
      check_empty "no match for id=3" r

    let t07 () =
      let r = eval_query "/root/*/b" sample_xml_str in
      check_count "/root/*/b" 2 r

    let t08 () =
      let r = eval_query "/root/**/b" sample_xml_str in
      check_count "/root/**/b" 3 r

    let t09 () =
      let r = eval_query "/**/f/b" sample_xml_str in
      check_count "/**/f/b" 1 r;
      check_has_element "name is b" "b" r

    let t10 () =
      let r = eval_query "/root/e/**/b" sample_xml_str in
      check_count "/root/e/**/b" 1 r;
      check_has_element "name is b" "b" r

    let t11 () =
      let r = eval_query "/**/d" sample_xml_str in
      check_count "/**/d" 1 r;
      check_has_element "name is d" "d" r

    let t12 () =
      let r = eval_query "/root/a/nonexistent" sample_xml_str in
      check_empty "no match for nonexistent" r

    (* t13: /root/a[2]/b — index out of bounds *)
    let t13 () =
      let r = eval_query "/root/a[2]/b" sample_xml_str in
      check_empty "no match for index 2" r

    let t14 () =
      let r = eval_query "/**/b@lang=\"en\"" sample_xml_str in
      check_count "/**/b@lang=\"en\"" 1 r;
      check_has_attr_value "lang=en" "lang" "en" r

    let t15 () =
      let r = eval_query "/**/b@lang=\"fr\"" sample_xml_str in
      check_empty "no match for lang=fr" r

    (* Wildcard with attributes — /* matches at depth 2 only *)
    let t16 () =
      let r = eval_query "/*@id" sample_xml_str in
      check_count "/*@id" 2 r

    let t17 () =
      let r = eval_query "/*@id=\"2\"" sample_xml_str in
      check_count "/*@id=\"2\"" 1 r;
      check_has_attr_value "id=2" "id" "2" r

    let t18 () =
      let r = eval_query "/*@type" sample_xml_str in
      check_count "/*@type" 2 r

    let t19 () =
      let r = eval_query "/*@type=\"magic\"" sample_xml_str in
      check_count "/*@type=\"magic\"" 2 r;
      check_has_attr_value "type=magic" "type" "magic" r

    let t20 () =
      let r = eval_query "/*@type=\"nonexistent\"" sample_xml_str in
      check_empty "no match for type=nonexistent" r

    (* MultiWildcard with attributes *)
    let t21 () =
      let r = eval_query "/**@id" sample_xml_str in
      check_count "/**@id" 4 r

    let t22 () =
      let r = eval_query "/**@id=\"2\"" sample_xml_str in
      check_count "/**@id=\"2\"" 1 r;
      check_has_attr_value "id=2" "id" "2" r

    let t23 () =
      let r = eval_query "/**@type" sample_xml_str in
      check_count "/**@type" 2 r

    let t24 () =
      let r = eval_query "/**@type=\"magic\"" sample_xml_str in
      check_count "/**@type=\"magic\"" 2 r;
      check_has_attr_value "type=magic" "type" "magic" r

    let t25 () =
      let r = eval_query "/**@type=\"nonexistent\"" sample_xml_str in
      check_empty "no match" r

    let t26 () =
      let r = eval_query "/**@type/child" sample_xml_str in
      check_count "/**@type/child" 1 r;
      check_has_attr_value "child has id=s-child" "id" "s-child" r
  end in
  [
    ( "/root/a/b", `Quick, T.t01 );
    ( "/root/a[0]/b", `Quick, T.t02 );
    ( "/root/a[1]/b", `Quick, T.t03 );
    ( "/root/a@id=\"1\"/c", `Quick, T.t04 );
    ( "/root/a@id=\"2\"/d", `Quick, T.t05 );
    ( "/root/a@id=\"3\"/d", `Quick, T.t06 );
    ( "/root/*/b", `Quick, T.t07 );
    ( "/root/**/b", `Quick, T.t08 );
    ( "/**/f/b", `Quick, T.t09 );
    ( "/root/e/**/b", `Quick, T.t10 );
    ( "/**/d", `Quick, T.t11 );
    ( "/root/a/nonexistent", `Quick, T.t12 );
    ( "/root/a[2]/b", `Quick, T.t13 );
    ( "/**/b@lang=\"en\"", `Quick, T.t14 );
    ( "/**/b@lang=\"fr\"", `Quick, T.t15 );
    ( "/*@id", `Quick, T.t16 );
    ( "/*@id=\"2\"", `Quick, T.t17 );
    ( "/*@type", `Quick, T.t18 );
    ( "/*@type=\"magic\"", `Quick, T.t19 );
    ( "/*@type=\"nonexistent\"", `Quick, T.t20 );
    ( "/**@id", `Quick, T.t21 );
    ( "/**@id=\"2\"", `Quick, T.t22 );
    ( "/**@type", `Quick, T.t23 );
    ( "/**@type=\"magic\"", `Quick, T.t24 );
    ( "/**@type=\"nonexistent\"", `Quick, T.t25 );
    ( "/**@type/child", `Quick, T.t26 );
  ]

(* --- Section: find_all (from test_upath.ml) --- *)

let find_all_tests =
  let module T = struct
    let t01 () =
      let r = eval_query "/root/a/b" sample_xml_str in
      check_count "/root/a/b" 2 r

    let t02 () =
      let r = eval_query "/root/a@id=\"1\"/b" sample_xml_str in
      check_count "/root/a@id=\"1\"/b" 1 r

    let t03 () =
      let r = eval_query "/root/a@id=\"2\"/b" sample_xml_str in
      check_count "/root/a@id=\"2\"/b" 1 r

    let t04 () =
      let r = eval_query "/root/*/b" sample_xml_str in
      check_count "/root/*/b" 2 r

    let t05 () =
      let r = eval_query "/root/nonexistent" sample_xml_str in
      check_empty "/root/nonexistent" r

    let t06 () =
      let r = eval_query "/root/a/nonexistent" sample_xml_str in
      check_empty "/root/a/nonexistent" r

    let t07 () =
      let r = eval_query "/root/**/b" sample_xml_str in
      check_count "/root/**/b" 3 r

    let t08 () =
      (* /*@id matches at depth 2 only *)
      let r = eval_query "/*@id" sample_xml_str in
      check_count "/*@id" 2 r

    let t09 () =
      let r = eval_query "/*@type" sample_xml_str in
      check_count "/*@type" 2 r

    let t10 () =
      let r = eval_query "/**@id" sample_xml_str in
      check_count "/**@id" 4 r

    let t11 () =
      let r = eval_query "/**@type" sample_xml_str in
      check_count "/**@type" 2 r

    let t12 () =
      let r = eval_query "/**@type/child" sample_xml_str in
      check_count "/**@type/child" 1 r

    let t13 () =
      let r = eval_query "/**/child" sample_xml_str in
      check_count "/**/child" 3 r

    let t14 () =
      let r = eval_query "/**@lang/nonexistent" sample_xml_str in
      check_empty "no match" r
  end in
  [
    ( "/root/a/b", `Quick, T.t01 );
    ( "/root/a@id=\"1\"/b", `Quick, T.t02 );
    ( "/root/a@id=\"2\"/b", `Quick, T.t03 );
    ( "/root/*/b", `Quick, T.t04 );
    ( "/root/nonexistent", `Quick, T.t05 );
    ( "/root/a/nonexistent", `Quick, T.t06 );
    ( "/root/**/b", `Quick, T.t07 );
    ( "/*@id", `Quick, T.t08 );
    ( "/*@type", `Quick, T.t09 );
    ( "/**@id", `Quick, T.t10 );
    ( "/**@type", `Quick, T.t11 );
    ( "/**@type/child", `Quick, T.t12 );
    ( "/**/child", `Quick, T.t13 );
    ( "/**@lang/nonexistent", `Quick, T.t14 );
  ]

(* --- Section: find_path_nested + find_all_nested (from test_upath.ml) --- *)

let nested_tests =
  let module T = struct
    let find_path_01 () =
      let r = eval_query "/root/a/b" sample_xml_nested_str in
      check_count "/root/a/b" 1 r;
      check_has_element "name is b" "b" r

    let find_path_02 () =
      let r = eval_query "/root/a/d/b" sample_xml_nested_str in
      check_count "/root/a/d/b" 1 r;
      check_has_element "name is b" "b" r

    let find_path_03 () =
      let r = eval_query "/root/**/b" sample_xml_nested_str in
      check_count "/root/**/b" 2 r;
      check_has_element "all are b" "b" r

    let find_path_04 () =
      (* Index[1] selects second c — the one with v=2 *)
      let r = eval_query "/root/a/b/c[1]" sample_xml_nested_str in
      check_count "/root/a/b/c[1]" 1 r;
      check_has_attr_value "v=2" "v" "2" r

    let find_path_05 () =
      (* Index[1] on a selects second a — only a@id=2 has d/b *)
      let r = eval_query "/root/a[1]/d/b" sample_xml_nested_str in
      check_count "/root/a[1]/d/b" 1 r;
      check_has_element "name is b" "b" r

    let find_all_01 () =
      let r = eval_query "/root/a/b" sample_xml_nested_str in
      check_count "/root/a/b" 1 r

    let find_all_02 () =
      let r = eval_query "/root/**/b" sample_xml_nested_str in
      check_count "/root/**/b" 2 r

    let find_all_03 () =
      let r = eval_query "/root/**/c" sample_xml_nested_str in
      check_count "/root/**/c" 3 r

    let find_all_04 () =
      let r = eval_query "/root/a/b/c" sample_xml_nested_str in
      check_count "/root/a/b/c" 2 r

    let find_all_05 () =
      (* Index[0] selects first a — only a@id=1's c's *)
      let r = eval_query "/root/a[0]/**/c" sample_xml_nested_str in
      check_count "/root/a[0]/**/c" 2 r

    let find_all_06 () =
      let r = eval_query "/root/a[1]/**/c" sample_xml_nested_str in
      check_count "/root/a[1]/**/c" 1 r
  end in
  [
    ( "find_path: /root/a/b", `Quick, T.find_path_01 );
    ( "find_path: /root/a/d/b", `Quick, T.find_path_02 );
    ( "find_path: /root/**/b", `Quick, T.find_path_03 );
    ( "find_path: /root/a/b/c[1]", `Quick, T.find_path_04 );
    ( "find_path: /root/a[1]/d/b", `Quick, T.find_path_05 );
    ( "find_all: /root/a/b", `Quick, T.find_all_01 );
    ( "find_all: /root/**/b", `Quick, T.find_all_02 );
    ( "find_all: /root/**/c", `Quick, T.find_all_03 );
    ( "find_all: /root/a/b/c", `Quick, T.find_all_04 );
    ( "find_all: /root/a[0]/**/c", `Quick, T.find_all_05 );
    ( "find_all: /root/a[1]/**/c", `Quick, T.find_all_06 );
  ]

(* --- Section: find_attr (from test_upath.ml) --- *)

let find_attr_tests =
  let module T = struct
    let t01 () =
      let r = eval_query "/root/a/c@val" sample_xml_str in
      check_only_attr_value "val attribute" "val" "test" r

    let t02 () =
      let r = eval_query "/root/a@id" sample_xml_str in
      check_count "a@id matches" 2 r;
      check_has_attr_value "has id=1" "id" "1" r

    let t03 () =
      let r = eval_query "/root/nonexistent@attr" sample_xml_str in
      check_empty "no match" r

    let t04 () =
      let r = eval_query "/root/a/b@nonexistent" sample_xml_str in
      check_empty "no match for nonexistent attr" r

    let t05 () =
      (* find_attr_opt("/root/a", "id") — match a elements, extract id attr *)
      let r = eval_query "/root/a" sample_xml_str in
      check_count "/root/a" 2 r;
      check_has_attr_value "has id=1" "id" "1" r

    let t06 () =
      (* find_attr_opt("/root/a/c", "val") — match c under a, extract val *)
      let r = eval_query "/root/a/c" sample_xml_str in
      check_only_attr_value "val attribute" "val" "test" r
  end in
  [
    ( "/root/a/c@val", `Quick, T.t01 );
    ( "/root/a@id", `Quick, T.t02 );
    ( "/root/nonexistent@attr", `Quick, T.t03 );
    ( "/root/a/b@nonexistent", `Quick, T.t04 );
    ( "/root/a -> id", `Quick, T.t05 );
    ( "/root/a/c -> val", `Quick, T.t06 );
  ]

(* --- Section: wildcard (from test_wildcard.ml) --- *)

let wildcard_tests =
  let module T = struct
    let t01 () =
      let r = eval_query "/root/*/child" wildcard_xml_str in
      check_count "/root/*/child" 1 r;
      check_has_element "name is child" "child" r

    let t02 () =
      let r = eval_query "/root/**/child" wildcard_xml_str in
      check_count "/root/**/child" 2 r;
      check_has_element "all are child" "child" r
  end in
  [
    ( "/root/*/child", `Quick, T.t01 );
    ( "/root/**/child", `Quick, T.t02 );
  ]

(* --- Section: complex_find + complex_find_all (from test_complex.ml) --- *)

let complex_tests =
  let module T = struct
    let find_01 () =
      let r =
        eval_query
          "/archive/library@name=\"music\"/section@id=\"A\"/artist/album@year=\"2022\"/track"
          complex_xml_str
      in
      check_count "deep path with attrs" 1 r;
      check_has_element "name is track" "track" r

    let find_02 () =
      let r =
        eval_query "/**/album@year=\"2021\"/track" complex_xml_str
      in
      check_count "multiwildcard + attr" 1 r;
      check_has_element "name is track" "track" r

    let find_03 () =
      (* Index[1] selects second library (pictures), photo[0] selects first photo *)
      let r =
        eval_query "/archive/library[1]/*/photo[0]" complex_xml_str
      in
      check_count "index + wildcard" 1 r

    let find_04 () =
      let r =
        eval_query "/**/track@feat=\"Artist2\"" complex_xml_str
      in
      check_count "track with feat attr" 1 r;
      check_has_attr_value "feat=Artist2" "feat" "Artist2" r

    let find_05 () =
      let r =
        eval_query "/archive/library@name=\"audiobooks\"/**/track" complex_xml_str
      in
      check_empty "no match for audiobooks library" r

    let find_06 () =
      (* Index[1] selects section B, track[0] selects first track under that path *)
      let r =
        eval_query
          "/archive/library/section[1]/artist/album/track[0]"
          complex_xml_str
      in
      check_count "index on section + track" 1 r

    let find_07 () =
      (* Index[2] selects 3rd artist (Artist3), track[1] selects 2nd track *)
      let r =
        eval_query "/**/artist[2]/album/track[1]" complex_xml_str
      in
      check_count "deep index query" 1 r;
      check_has_attr_value "contains track no=2" "no" "2" r

    let find_08 () =
      (* Multiple index + attr constraints: section[0]=A, artist=Artist1,
         album[1]=Album1.2, track[0]=TrackA *)
      let r =
        eval_query
          "/archive/library@name=\"music\"/section[0]/artist@name=\"Artist1\"/album[1]/track[0]"
          complex_xml_str
      in
      check_count "multi-index multi-attr" 1 r;
      check_has_attr_value "track no=1" "no" "1" r

    let find_all_01 () =
      let r = eval_query "/**/track" complex_xml_str in
      check_count "/**/track" 6 r

    let find_all_02 () =
      let r = eval_query "/**/album@year=\"2022\"" complex_xml_str in
      check_count "/**/album@year=\"2022\"" 2 r

    let find_all_03 () =
      let r =
        eval_query "/archive/library@name=\"music\"/**/artist" complex_xml_str
      in
      check_count "artists in music library" 3 r

    let find_all_04 () =
      let r =
        eval_query "/**/artist@name=\"Artist1\"/**/track" complex_xml_str
      in
      check_count "tracks under Artist1" 3 r
  end in
  [
    ( "find: deep attrs", `Quick, T.find_01 );
    ( "find: multiwildcard + attr", `Quick, T.find_02 );
    ( "find: index + wildcard", `Quick, T.find_03 );
    ( "find: track@feat", `Quick, T.find_04 );
    ( "find: audiobooks (no match)", `Quick, T.find_05 );
    ( "find: index on section+track", `Quick, T.find_06 );
    ( "find: deep index", `Quick, T.find_07 );
    ( "find: multi-index multi-attr", `Quick, T.find_08 );
    ( "find_all: /**/track", `Quick, T.find_all_01 );
    ( "find_all: album@year=\"2022\"", `Quick, T.find_all_02 );
    ( "find_all: artists in music", `Quick, T.find_all_03 );
    ( "find_all: tracks under Artist1", `Quick, T.find_all_04 );
  ]

(* --- Main --- *)

let () =
  Alcotest.run "Upath2 Streaming" [
    "find_path",
    List.map (fun (name, speed, f) -> Alcotest.test_case name speed f)
      find_path_tests;
    "find_all",
    List.map (fun (name, speed, f) -> Alcotest.test_case name speed f)
      find_all_tests;
    "nested",
    List.map (fun (name, speed, f) -> Alcotest.test_case name speed f)
      nested_tests;
    "find_attr",
    List.map (fun (name, speed, f) -> Alcotest.test_case name speed f)
      find_attr_tests;
    "wildcard",
    List.map (fun (name, speed, f) -> Alcotest.test_case name speed f)
      wildcard_tests;
    "complex_find",
    List.map (fun (name, speed, f) -> Alcotest.test_case name speed f)
      complex_tests;
  ]
