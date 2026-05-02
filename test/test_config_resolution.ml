open Alcotest
open Alsdiff_output.Config

let make_temp_dir prefix =
  let path = Filename.temp_file prefix "" in
  Sys.remove path;
  Unix.mkdir path 0o755;
  path

let ensure_dir path =
  if not (Sys.file_exists path) then
    Unix.mkdir path 0o755

let write_config path cfg =
  Out_channel.with_open_text path (fun oc ->
      output_string oc (detail_config_to_string_with_schema cfg);
      output_char oc '\n')

let test_explicit_config_beats_preset_and_auto () =
  let root = make_temp_dir "alsdiff-config-explicit-" in
  let repo = Filename.concat root "repo" in
  let project = Filename.concat repo "project" in
  ensure_dir repo;
  ensure_dir project;
  ensure_dir (Filename.concat repo ".git");
  let auto_config = Filename.concat project ".alsdiff.json" in
  let explicit_config = Filename.concat root "explicit.json" in
  write_config auto_config (with_prefixes ~added:"AUTO" ~removed:"-" ~modified:"*" ~unchanged:"=" quiet);
  write_config explicit_config (with_prefixes ~added:"EXPLICIT" ~removed:"-" ~modified:"*" ~unchanged:"=" quiet);
  match resolve_detail_config
          ~cwd:repo
          ~default_config:quiet
          ~reference_path:(Filename.concat project "song.als")
          ~config_file:(Some explicit_config)
          ~preset_config:(Some (with_prefixes ~added:"PRESET" ~removed:"-" ~modified:"*" ~unchanged:"=" quiet))
          ()
  with
  | Ok cfg -> check string "preset beats explicit config" "PRESET" cfg.prefix_added
  | Error msg -> fail msg

let test_preset_beats_auto_discovery () =
  let root = make_temp_dir "alsdiff-config-preset-" in
  let repo = Filename.concat root "repo" in
  let project = Filename.concat repo "project" in
  ensure_dir repo;
  ensure_dir project;
  ensure_dir (Filename.concat repo ".git");
  let auto_config = Filename.concat project ".alsdiff.json" in
  write_config auto_config (with_prefixes ~added:"AUTO" ~removed:"-" ~modified:"*" ~unchanged:"=" quiet);
  match resolve_detail_config
          ~cwd:repo
          ~default_config:quiet
          ~reference_path:(Filename.concat project "song.als")
          ~config_file:None
          ~preset_config:(Some (with_prefixes ~added:"PRESET" ~removed:"-" ~modified:"*" ~unchanged:"=" quiet))
          ()
  with
  | Ok cfg -> check string "preset wins" "PRESET" cfg.prefix_added
  | Error msg -> fail msg

let test_discover_config_file_search_order () =
  let root = make_temp_dir "alsdiff-config-discover-" in
  let repo = Filename.concat root "repo" in
  let project = Filename.concat repo "project" in
  let nongit = Filename.concat root "nongit" in
  let home = Filename.concat root "home" in
  ensure_dir repo;
  ensure_dir project;
  ensure_dir nongit;
  ensure_dir home;
  ensure_dir (Filename.concat repo ".git");
  let project_config = Filename.concat project ".alsdiff.json" in
  let repo_config = Filename.concat repo ".alsdiff.json" in
  let home_config = Filename.concat home ".alsdiff.json" in
  write_config project_config quiet;
  write_config repo_config full;
  write_config home_config compact;

  check (option string) "project config first"
    (Some project_config)
    (discover_config_file
       ~cwd:project
       ~home_dir:home
       ~reference_path:(Filename.concat project "song.als")
       ());

  check (option string) "git root config second"
    (Some repo_config)
    (discover_config_file
       ~cwd:project
       ~home_dir:home
       ~reference_path:(Filename.concat project "other/song.als")
       ());

  check (option string) "home config last"
    (Some home_config)
    (discover_config_file
       ~cwd:nongit
       ~home_dir:home
       ~reference_path:(Filename.concat nongit "song.als")
       ())

let () =
  run "Config resolution" [
    "precedence", [
      test_case "preset beats explicit config and auto-discovery" `Quick test_explicit_config_beats_preset_and_auto;
      test_case "preset beats auto-discovery" `Quick test_preset_beats_auto_discovery;
    ];
    "discovery", [
      test_case "search order stays unchanged" `Quick test_discover_config_file_search_order;
    ];
  ]
