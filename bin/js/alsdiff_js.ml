open Alsdiff_base
open Alsdiff_live
open Alsdiff_output
open Config
open Lwt.Syntax
open View_model

let load_liveset file =
  let xml = File.open_als file in
  Lwt.return (Liveset.create xml file)

let create_views (change : (Liveset.t, Liveset.Patch.t) Diff.structured_change)
  : View_model.view list =
  let item = View_model.create_liveset_item change in
  [View_model.Item item]

type output_mode = Tree | Stats

type preset = [ `Compact | `Composer | `Full | `Inline | `Mixing | `Quiet | `Verbose ]

type config = {
  positional_args: string list;
  git_mode: bool;
  output_mode: output_mode;
  config_file: string option;
  preset: preset option;
  dump_preset: preset option;
  prefix_added: string option;
  prefix_removed: string option;
  prefix_modified: string option;
  prefix_unchanged: string option;
  note_name_style: note_display_style option;
  max_collection_items: int option;
  dump_schema: bool;
  validate_config: string option;
}

type git_args = {
  path: string;
  old_file: string;
  new_file: string;
}

let parse_git_args args =
  match args with
  | [path; old_file; _old_hex; _old_mode; new_file; _new_hex; _new_mode] ->
    Ok { path; old_file; new_file }
  | _ ->
    Error "Git mode requires exactly 7 positional arguments: path old-file old-hex old-mode new-file new-hex new-mode"

let parse_preset_args = function
  | `Compact -> Text_renderer.compact
  | `Composer -> Text_renderer.composer
  | `Full -> Text_renderer.full
  | `Inline -> Text_renderer.inline
  | `Mixing -> Text_renderer.mixing
  | `Quiet -> Text_renderer.quiet
  | `Verbose -> Text_renderer.verbose

let load_config_from_json file_path =
  match Text_renderer.load_and_validate_config file_path with
  | Ok cfg -> cfg
  | Error msg ->
    Fmt.epr "%s@." msg;
    exit 1

let find_git_root () =
  let rec search path =
    if Sys.file_exists (Filename.concat path ".git") then
      Some path
    else
      let parent = Filename.dirname path in
      if parent = path then None
      else search parent
  in
  search (Sys.getcwd ())

let get_home_dir () =
  match Sys.getenv_opt "HOME" with
  | Some home -> Some home
  | None ->
    (* Fallback for Windows systems *)
    match Sys.getenv_opt "USERPROFILE" with
    | Some userprofile -> Some userprofile
    | None -> None

let discover_config_file ~reference_path =
  (* Try reference_path directory config first - highest priority *)
  let check_path_dir_config () =
    let path_dir = Filename.dirname reference_path in
    let path_config = Filename.concat path_dir ".alsdiff.json" in
    if Sys.file_exists path_config then Some path_config else None
  in
  (* Try git root config *)
  let check_git_config () =
    match find_git_root () with
    | Some git_root ->
      let git_config = Filename.concat git_root ".alsdiff.json" in
      if Sys.file_exists git_config then Some git_config else None
    | None -> None
  in
  (* Try home directory config *)
  let check_home_config () =
    match get_home_dir () with
    | Some home ->
      let home_config = Filename.concat home ".alsdiff.json" in
      if Sys.file_exists home_config then Some home_config else None
    | None -> None
  in
  (* Priority: path dir > git config > home config *)
  match check_path_dir_config () with
  | Some _ as result -> result
  | None ->
    match check_git_config () with
    | Some _ as result -> result
    | None -> check_home_config ()

let load_and_report_config config_path =
  Fmt.pr "Loading configuration from %s@." config_path;
  load_config_from_json config_path

let build_base_renderer_config ~default_config ~reference_path config =
  match config.config_file with
  | Some config_path ->
    load_config_from_json config_path
  | None ->
    match config.preset with
    | Some preset -> parse_preset_args preset
    | None ->
      match discover_config_file ~reference_path with
      | Some auto_config -> load_and_report_config auto_config
      | None -> default_config

let stats_incompatible_flags_provided config =
  config.prefix_added <> None
  || config.prefix_removed <> None
  || config.prefix_modified <> None
  || config.prefix_unchanged <> None
  || config.note_name_style <> None
  || config.max_collection_items <> None


let render_stats ~config ~reference_path views =
  let base_renderer_config = build_base_renderer_config ~default_config:stats_default ~reference_path config in
  (* Stats mode doesn't use prefix/note_style/max_items, so no merging needed *)
  Stats_renderer.render base_renderer_config views

let render_tree ~config ~reference_path views =
  let base_renderer_config = build_base_renderer_config ~default_config:Text_renderer.quiet ~reference_path config in
  let renderer_config = {
    base_renderer_config with
    prefix_added = (match config.prefix_added with Some s -> s | None -> base_renderer_config.prefix_added);
    prefix_removed = (match config.prefix_removed with Some s -> s | None -> base_renderer_config.prefix_removed);
    prefix_modified = (match config.prefix_modified with Some s -> s | None -> base_renderer_config.prefix_modified);
    prefix_unchanged = (match config.prefix_unchanged with Some s -> s | None -> base_renderer_config.prefix_unchanged);
    note_name_style = (match config.note_name_style with Some s -> s | None -> base_renderer_config.note_name_style);
    max_collection_items = (match config.max_collection_items with Some n -> Some n | None -> base_renderer_config.max_collection_items);
  } in
  Text_renderer.render renderer_config views

let diff_cmd ~config =
  let* () = Lwt.return_unit in
  if config.output_mode = Stats && stats_incompatible_flags_provided config then begin
    Fmt.epr "Error: --mode stats is incompatible with --prefix-*, \
             --note-name-style, and --max-collection-items@.";
    let exit_code = if config.git_mode then 2 else 1 in
    Lwt.return exit_code
  end else begin
    let file1, file2, reference_path =
      if config.git_mode then
        match parse_git_args config.positional_args with
        | Error msg -> failwith msg
        | Ok git_args -> (git_args.old_file, git_args.new_file, git_args.path)
      else
        match config.positional_args with
        | [f1; f2] -> (f1, f2, f2)
        | _ -> failwith "FILE1.als and FILE2.als are required for diff"
    in

    let* liveset1 = load_liveset file1 in
    let* liveset2 = load_liveset file2 in

    let liveset_patch = Liveset.diff liveset1 liveset2 in
    let has_changes = not (Liveset.Patch.is_empty liveset_patch) in

    let liveset_change =
      if has_changes then
        `Modified liveset_patch
      else
        `Unchanged
    in

    let views = create_views liveset_change in

    let output = match config.output_mode with
      | Stats -> render_stats ~config ~reference_path views
      | Tree -> render_tree ~config ~reference_path views
    in

    Fmt.pr "%s@." output;

    let exit_code =
      if config.git_mode then
        if has_changes then 1 else 0
      else
        0
    in
    Lwt.return exit_code
  end

type parse_result = [ `Run of config | `Show_help | `Show_version ]

type parse_error = {
  message: string;
  exit_code: int;
}

let js_version = "dev-js"

let default_config = {
  positional_args = [];
  git_mode = false;
  output_mode = Tree;
  config_file = None;
  preset = None;
  dump_preset = None;
  prefix_added = None;
  prefix_removed = None;
  prefix_modified = None;
  prefix_unchanged = None;
  note_name_style = None;
  max_collection_items = None;
  dump_schema = false;
  validate_config = None;
}

let usage =
  "Usage: alsdiff [OPTIONS] FILE1.als FILE2.als\n\
   \n\
   Compare two Ableton Live Set (.als) files and show differences.\n\
   \n\
   Git mode:\n\
   \  alsdiff --git path old-file old-hex old-mode new-file new-hex new-mode\n\
   \n\
   Options:\n\
   \  --mode tree|stats\n\
   \  --preset compact|composer|full|inline|mixing|quiet|verbose\n\
   \  --config FILE\n\
   \  --dump-preset PRESET\n\
   \  --dump-schema\n\
   \  --validate-config FILE\n\
   \  --git\n\
   \  --prefix-added PREFIX\n\
   \  --prefix-removed PREFIX\n\
   \  --prefix-modified PREFIX\n\
   \  --prefix-unchanged PREFIX\n\
   \  --note-name-style Sharp|Flat\n\
   \  --max-collection-items N\n\
   \  --version\n\
   \  --help"

let starts_with ~prefix s =
  let prefix_len = String.length prefix in
  String.length s >= prefix_len && String.sub s 0 prefix_len = prefix

let split_long_option arg =
  match String.index_opt arg '=' with
  | None -> (arg, None)
  | Some idx ->
    let name = String.sub arg 0 idx in
    let value = String.sub arg (idx + 1) (String.length arg - idx - 1) in
    (name, Some value)

let option_requires_value name inline_value rest =
  match inline_value with
  | Some value -> Ok (value, rest)
  | None ->
    match rest with
    | value :: rest' -> Ok (value, rest')
    | [] ->
      Error {
        message = Fmt.str "Error: option %s requires a value." name;
        exit_code = 1;
      }

let parse_output_mode value =
  match value with
  | "tree" -> Ok Tree
  | "stats" -> Ok Stats
  | _ ->
    Error {
      message = Fmt.str "Error: invalid value '%s' for --mode (expected tree|stats)." value;
      exit_code = 1;
    }

let parse_preset value =
  match value with
  | "compact" -> Ok `Compact
  | "composer" -> Ok `Composer
  | "full" -> Ok `Full
  | "inline" -> Ok `Inline
  | "mixing" -> Ok `Mixing
  | "quiet" -> Ok `Quiet
  | "verbose" -> Ok `Verbose
  | _ ->
    Error {
      message =
        Fmt.str
          "Error: invalid value '%s' for preset (expected compact|composer|full|inline|mixing|quiet|verbose)."
          value;
      exit_code = 1;
    }

let parse_note_name_style value =
  match value with
  | "Sharp" -> Ok Sharp
  | "Flat" -> Ok Flat
  | _ ->
    Error {
      message = Fmt.str "Error: invalid value '%s' for --note-name-style (expected Sharp|Flat)." value;
      exit_code = 1;
    }

let parse_max_collection_items value =
  match int_of_string_opt value with
  | Some n -> Ok n
  | None ->
    Error {
      message = Fmt.str "Error: invalid integer '%s' for --max-collection-items." value;
      exit_code = 1;
    }

let option_without_value name inline_value =
  match inline_value with
  | None -> Ok ()
  | Some _ ->
    Error {
      message = Fmt.str "Error: option %s does not take a value." name;
      exit_code = 1;
    }

let parse_args (argv : string list) : (parse_result, parse_error) result =
  let rec loop cfg positional = function
    | [] ->
      Ok (`Run { cfg with positional_args = List.rev positional })
    | "--" :: rest ->
      Ok (`Run { cfg with positional_args = List.rev_append positional rest })
    | ("--help" | "-h") :: _ ->
      Ok `Show_help
    | "--version" :: _ ->
      Ok `Show_version
    | arg :: rest when starts_with ~prefix:"--" arg ->
      let name, inline_value = split_long_option arg in
      begin
        match name with
        | "--git" ->
          (match option_without_value name inline_value with
           | Error _ as error -> error
           | Ok () -> loop { cfg with git_mode = true } positional rest)
        | "--dump-schema" ->
          (match option_without_value name inline_value with
           | Error _ as error -> error
           | Ok () -> loop { cfg with dump_schema = true } positional rest)
        | "--config" ->
          (match option_requires_value name inline_value rest with
           | Error _ as error -> error
           | Ok (value, rest') -> loop { cfg with config_file = Some value } positional rest')
        | "--validate-config" ->
          (match option_requires_value name inline_value rest with
           | Error _ as error -> error
           | Ok (value, rest') ->
             loop { cfg with validate_config = Some value } positional rest')
        | "--mode" ->
          (match option_requires_value name inline_value rest with
           | Error _ as error -> error
           | Ok (value, rest') ->
             match parse_output_mode value with
             | Error _ as error -> error
             | Ok output_mode -> loop { cfg with output_mode } positional rest')
        | "--preset" ->
          (match option_requires_value name inline_value rest with
           | Error _ as error -> error
           | Ok (value, rest') ->
             match parse_preset value with
             | Error _ as error -> error
             | Ok preset -> loop { cfg with preset = Some preset } positional rest')
        | "--dump-preset" ->
          (match option_requires_value name inline_value rest with
           | Error _ as error -> error
           | Ok (value, rest') ->
             match parse_preset value with
             | Error _ as error -> error
             | Ok preset -> loop { cfg with dump_preset = Some preset } positional rest')
        | "--prefix-added" ->
          (match option_requires_value name inline_value rest with
           | Error _ as error -> error
           | Ok (value, rest') -> loop { cfg with prefix_added = Some value } positional rest')
        | "--prefix-removed" ->
          (match option_requires_value name inline_value rest with
           | Error _ as error -> error
           | Ok (value, rest') ->
             loop { cfg with prefix_removed = Some value } positional rest')
        | "--prefix-modified" ->
          (match option_requires_value name inline_value rest with
           | Error _ as error -> error
           | Ok (value, rest') ->
             loop { cfg with prefix_modified = Some value } positional rest')
        | "--prefix-unchanged" ->
          (match option_requires_value name inline_value rest with
           | Error _ as error -> error
           | Ok (value, rest') ->
             loop { cfg with prefix_unchanged = Some value } positional rest')
        | "--note-name-style" ->
          (match option_requires_value name inline_value rest with
           | Error _ as error -> error
           | Ok (value, rest') ->
             match parse_note_name_style value with
             | Error _ as error -> error
             | Ok note_name_style ->
               loop { cfg with note_name_style = Some note_name_style } positional rest')
        | "--max-collection-items" ->
          (match option_requires_value name inline_value rest with
           | Error _ as error -> error
           | Ok (value, rest') ->
             match parse_max_collection_items value with
             | Error _ as error -> error
             | Ok max_collection_items ->
               loop { cfg with max_collection_items = Some max_collection_items } positional
                 rest')
        | "--help" ->
          (match option_without_value name inline_value with
           | Error _ as error -> error
           | Ok () -> Ok `Show_help)
        | "--version" ->
          (match option_without_value name inline_value with
           | Error _ as error -> error
           | Ok () -> Ok `Show_version)
        | _ ->
          Error {
            message = Fmt.str "Error: unknown option '%s'." name;
            exit_code = 1;
          }
      end
    | arg :: _ when starts_with ~prefix:"-" arg ->
      Error {
        message = Fmt.str "Error: unknown option '%s'." arg;
        exit_code = 1;
      }
    | arg :: rest ->
      loop cfg (arg :: positional) rest
  in
  loop default_config [] argv

let config_ref = ref None

let main () =
  Printexc.record_backtrace true;

  (* Helper to get error exit code based on mode *)
  let error_exit_code () =
    match !config_ref with
    | Some cfg when cfg.git_mode -> 2
    | _ -> 1
  in

  let run_cmd () =
    let args =
      match Array.to_list Sys.argv with
      | [] -> []
      | _prog :: rest -> rest
    in
    match parse_args args with
    | Ok `Show_help ->
      print_endline usage;
      Lwt.return 0
    | Ok `Show_version ->
      print_endline js_version;
      Lwt.return 0
    | Error { message; exit_code } ->
      Fmt.epr "%s@." message;
      Fmt.epr "Use --help for usage.@.";
      Lwt.return exit_code
    | Ok (`Run cfg) ->
      config_ref := Some cfg;
      (* Handle --dump-preset first *)
      (match cfg.dump_preset with
       | Some preset ->
         let preset_config = parse_preset_args preset in
         let json = Text_renderer.detail_config_to_yojson_with_schema preset_config in
         print_endline (Yojson.Safe.pretty_to_string json);
         Lwt.return 0
       | None ->
         (* Handle --validate-config *)
         (match cfg.validate_config with
          | Some config_path ->
            (match Text_renderer.validate_config_file config_path with
             | Ok () ->
               Fmt.pr "Configuration file %s is valid@." config_path;
               Lwt.return 0
             | Error msg ->
               Fmt.epr "%s@." msg;
               Lwt.return 1)
          | None ->
            (* Handle --dump-schema *)
            if cfg.dump_schema then begin
              print_endline (Text_renderer.detail_config_schema_to_string ());
              Lwt.return 0
            end else begin
              (* Normal diff operation - validate args based on mode *)
              let has_valid_args =
                if cfg.git_mode then
                  (* Git mode needs exactly 7 positional args *)
                  List.length cfg.positional_args = 7
                else
                  (* Normal mode needs exactly 2 positional args *)
                  List.length cfg.positional_args = 2
              in
              if has_valid_args then
                diff_cmd ~config:cfg
              else if cfg.git_mode then begin
                Fmt.epr "Error: --git mode requires exactly 7 positional arguments@.";
                Fmt.epr "Usage: alsdiff --git path old-file old-hex old-mode new-file new-hex new-mode@.";
                Lwt.return 2
              end else begin
                Fmt.epr "Error: FILE1.als and FILE2.als are required for diff@.";
                Fmt.epr "Use --dump-schema to generate configuration schema without files.@.";
                Fmt.epr "Use --dump-preset PRESET to dump a preset configuration as JSON.@.";
                Fmt.epr "Use --validate-config FILE to validate a configuration file.@.";
                Lwt.return 1
              end
            end))
  in

  Lwt.catch
    (fun () -> run_cmd ())
    (fun exn ->
       match exn with
       | File.File_error (file, msg) ->
         let bt = Printexc.get_backtrace () in
         Fmt.epr "Error: Failed to process file '%s': %s@." file msg;
         Fmt.epr "%s@." bt;
         Lwt.return (error_exit_code ())
       | Xml.Xml_error (xml, msg) ->
         let bt = Printexc.get_backtrace () in
         Fmt.epr "Error: Invalid XML format: %s@.%a@." msg Xml.pp xml;
         Fmt.epr "%s@." bt;
         Lwt.return (error_exit_code ())
       | Upath.Path_not_found (path, xml) ->
         let bt = Printexc.get_backtrace () in
         Fmt.epr "Error: Required path '%s' not found in @.%a@." path Xml.pp xml;
         Fmt.epr "%s@." bt;
         Lwt.return (error_exit_code ())
       | Sys_error msg ->
         let bt = Printexc.get_backtrace () in
         Fmt.epr "System Error: %s@." msg;
         Fmt.epr "%s@." bt;
         Lwt.return (error_exit_code ())
       | Failure msg ->
         let bt = Printexc.get_backtrace () in
         Fmt.epr "Error: %s@." msg;
         Fmt.epr "%s@." bt;
         Lwt.return (error_exit_code ())
       | exn ->
         let bt = Printexc.get_backtrace () in
         Fmt.epr "Unexpected error: %s@.%s@." (Printexc.to_string exn) bt;
         Fmt.epr "Please report this bug at https://github.com/krfantasy/alsdiff/issues@.";
         Lwt.return (error_exit_code ()))

let () =
  if !Sys.interactive then ()
  else
    (* For js_of_ocaml with Lwt, register a callback to handle the exit code *)
    Lwt.bind (main ()) (fun exit_code -> Lwt.return (exit exit_code)) |> ignore
