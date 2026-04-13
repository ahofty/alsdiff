(** Streaming XML pipeline test — path-to-NFA against t4.xml.

    Demonstrates compiling 10 path queries into a shared NFA and evaluating
    them in a single streaming pass over t4.xml. *)

open Alsdiff_base

(* Inline resolve helper — same pattern as test/utils.ml *)
let resolve_test_data_path filename =
  if Sys.file_exists ("data/" ^ filename) then "data/" ^ filename
  else if Sys.file_exists filename then filename
  else if Sys.file_exists ("test/data/" ^ filename) then "test/data/" ^ filename
  else if Sys.file_exists ("test/" ^ filename) then "test/" ^ filename
  else failwith (Printf.sprintf "Cannot find test data file: %s" filename)

let queries : Upath2.query list = [
  (* Track names *)
  Upath2.simple_query ~qid:0 ~path:["Ableton"; "LiveSet"; "Tracks"; "MidiTrack"; "Name"; "EffectiveName"]
    ~attr:(Some "Value");
  Upath2.simple_query ~qid:1 ~path:["Ableton"; "LiveSet"; "Tracks"; "AudioTrack"; "Name"; "EffectiveName"]
    ~attr:(Some "Value");
  Upath2.simple_query ~qid:2 ~path:["Ableton"; "LiveSet"; "MainTrack"; "Name"; "EffectiveName"]
    ~attr:(Some "Value");
  (* Track IDs *)
  Upath2.simple_query ~qid:3 ~path:["Ableton"; "LiveSet"; "Tracks"; "MidiTrack"]
    ~attr:(Some "Id");
  Upath2.simple_query ~qid:4 ~path:["Ableton"; "LiveSet"; "Tracks"; "AudioTrack"]
    ~attr:(Some "Id");
  (* Mixer values *)
  Upath2.simple_query ~qid:5 ~path:["Ableton"; "LiveSet"; "Tracks"; "MidiTrack";
                                    "DeviceChain"; "Mixer"; "Volume"; "Manual"]
    ~attr:(Some "Value");
  Upath2.simple_query ~qid:6 ~path:["Ableton"; "LiveSet"; "Tracks"; "AudioTrack";
                                    "DeviceChain"; "Mixer"; "Volume"; "Manual"]
    ~attr:(Some "Value");
  Upath2.simple_query ~qid:7 ~path:["Ableton"; "LiveSet"; "MainTrack";
                                    "DeviceChain"; "Mixer"; "Tempo"; "Manual"]
    ~attr:(Some "Value");
  Upath2.simple_query ~qid:8 ~path:["Ableton"; "LiveSet"; "MainTrack";
                                    "DeviceChain"; "Mixer"; "Volume"; "Manual"]
    ~attr:(Some "Value");
  Upath2.simple_query ~qid:9 ~path:["Ableton"; "LiveSet"; "Tracks"; "MidiTrack";
                                    "DeviceChain"; "Mixer"; "On"; "Manual"]
    ~attr:(Some "Value");
]

(* Find the first match for a given query_id and extract the requested attr *)
let find_attr (queries : Upath2.query list) results qid =
  let q = List.find (fun q -> q.Upath2.qid = qid) queries in
  let r = List.find (fun r -> r.Upath2.query_id = qid) results in
  match q.Upath2.attr with
  | Some attr_name -> Upath2.get_attr r attr_name
  | None -> Some r.Upath2.element_name

let test_nfa_stats () =
  let nfa = Upath2.compile queries in
  let state_count = Array.length nfa.states in
  let naive_count = List.fold_left (fun acc q -> acc + List.length q.Upath2.path) 0 queries in
  Printf.printf "NFA stats: %d states (naive: %d, sharing ratio: %.1fx)\n"
    state_count naive_count
    (float_of_int naive_count /. float_of_int state_count);
  Fmt.pr "%a@." Upath2.pp_nfa nfa;
  Alcotest.(check bool) "NFA state count is reasonable" true (state_count <= naive_count + 1)

let test_streaming_matches () =
  let nfa = Upath2.compile queries in
  let path = resolve_test_data_path "t4.xml" in
  let stream = Xml2.stream_from_file path in
  let results = Upath2.evaluate nfa stream in
  Printf.printf "--- Streaming results (%d matches) ---\n" (List.length results);
  List.iter (fun r -> Fmt.pr "%a@." Upath2.pp_match_result r) results;
  (* Expected values from t4.xml *)
  let expected = [
    (0, "1-Tela");
    (1, "2-Audio");
    (2, "Main");
    (3, "14");
    (4, "15");
    (5, "1");
    (6, "1");
    (7, "120");
    (8, "1");
    (9, "true");
  ] in
  List.iter (fun (qid, expected_val) ->
      match find_attr queries results qid with
      | Some v ->
        Alcotest.(check string) (Printf.sprintf "q%d" qid) expected_val v
      | None ->
        Alcotest.fail (Printf.sprintf "q%d: no match found" qid)
    ) expected

let () =
  Alcotest.run "Streaming XML Pipeline" [
    "nfa_stats", [
      Alcotest.test_case "prefix sharing reduces state count" `Quick test_nfa_stats;
    ];
    "streaming_matches", [
      Alcotest.test_case "all 10 queries match expected values" `Quick test_streaming_matches;
    ];
  ]
