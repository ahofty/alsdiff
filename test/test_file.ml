open Alcotest
open Alsdiff_base

let write_plain_file path contents =
  Out_channel.with_open_text path (fun oc -> output_string oc contents)

let write_gzip_file path contents =
  let oc = Gzip.open_out path in
  let bytes = Bytes.of_string contents in
  Fun.protect
    ~finally:(fun () -> Gzip.close_out oc)
    (fun () -> Gzip.output oc bytes 0 (Bytes.length bytes))

let contains_substring haystack needle =
  let haystack_len = String.length haystack in
  let needle_len = String.length needle in
  let rec loop i =
    if i + needle_len > haystack_len then false
    else if String.sub haystack i needle_len = needle then true
    else loop (i + 1)
  in
  needle_len = 0 || loop 0

let expect_file_error_contains expected f =
  try
    ignore (f () : 'a);
    fail ("Expected File_error containing: " ^ expected)
  with
  | File.File_error (_, msg) ->
    check bool "message contains expected text" true (contains_substring msg expected)

let make_temp_dir prefix =
  let path = Filename.temp_file prefix "" in
  Sys.remove path;
  Unix.mkdir path 0o755;
  path

let with_tmpdir dir f =
  let original = Sys.getenv_opt "TMPDIR" in
  let default_tmpdir = Filename.get_temp_dir_name () in
  Unix.putenv "TMPDIR" dir;
  Fun.protect
    ~finally:(fun () ->
        match original with
        | Some value -> Unix.putenv "TMPDIR" value
        | None -> Unix.putenv "TMPDIR" default_tmpdir)
    f

let sorted_entries dir =
  Sys.readdir dir |> Array.to_list |> List.sort String.compare

let test_missing_file_raises_file_error () =
  expect_file_error_contains "No such file" (fun () -> File.open_als "/tmp/alsdiff-does-not-exist.als")

let test_invalid_gzip_raises_file_error () =
  let path = Filename.temp_file "alsdiff-invalid-gzip-" ".als" in
  write_plain_file path "not gzip";
  expect_file_error_contains "Gzip error" (fun () -> File.decompress_als_to_string path)

let test_invalid_xml_raises_file_error () =
  let path = Filename.temp_file "alsdiff-invalid-xml-" ".als" in
  write_gzip_file path "<Ableton><LiveSet>";
  expect_file_error_contains "XML parsing error" (fun () -> File.open_als path)

let test_decompress_als_cleans_up_temp_file_on_failure () =
  let dir = make_temp_dir "alsdiff-file-test-" in
  let invalid_path = Filename.concat dir "invalid.als" in
  write_plain_file invalid_path "not gzip";
  let before = sorted_entries dir in
  with_tmpdir dir (fun () ->
      expect_file_error_contains "Gzip error" (fun () -> File.decompress_als invalid_path));
  let after = sorted_entries dir in
  check (list string) "no leaked temp xml file" before after

let () =
  run "File" [
    "errors", [
      test_case "missing file raises File_error" `Quick test_missing_file_raises_file_error;
      test_case "invalid gzip raises File_error" `Quick test_invalid_gzip_raises_file_error;
      test_case "invalid xml raises File_error" `Quick test_invalid_xml_raises_file_error;
      test_case "decompress_als cleans temp file on failure" `Quick test_decompress_als_cleans_up_temp_file_on_failure;
    ];
  ]
