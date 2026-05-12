(** Main browser library entry point for alsdiff

    This module provides a high-level JavaScript API for comparing Ableton Live Set
    files in web browsers.
*)

open Alsdiff_live
open Alsdiff_output
open Config
open Js_of_ocaml

exception File_error of string * string

let debug_enabled = ref false

let set_global_debug_flag (enabled : bool) : unit =
  Js.Unsafe.set Js.Unsafe.global (Js.string "__alsdiff_debug")
    (Js.Unsafe.inject (Js.bool enabled))

let console_call (method_name : string) (msg : string) : unit =
  let console = Js.Unsafe.get Js.Unsafe.global (Js.string "console") in
  let fn = Js.Unsafe.get console (Js.string method_name) in
  ignore
  @@ Js.Unsafe.call fn Js.undefined
    [| Js.Unsafe.inject (Js.string msg) |]

let debug_log (msg : string) : unit =
  if !debug_enabled then console_call "log" msg

let browser_yield () : unit Lwt.t =
  let result, wake = Lwt.wait () in
  let callback = Js.wrap_callback (fun () -> Lwt.wakeup wake (); Js.undefined) in
  ignore
  @@ Js.Unsafe.fun_call (Js.Unsafe.js_expr "setTimeout")
    [| Js.Unsafe.inject callback; Js.Unsafe.inject (Js.number_of_float 0.) |];
  result

type browser_file = {
  name: string;
  file_obj: Js.Unsafe.any;
}

let from_js_blob (blob : Js.Unsafe.any) (name : string) : browser_file =
  { name; file_obj = blob }

let from_js_file (file : Js.Unsafe.any) : browser_file =
  let name_prop = Js.Unsafe.get file (Js.string "name") in
  { name = Js.to_string (Js.Unsafe.coerce name_prop); file_obj = file }

(* File ID reference - avoids passing File objects through js_of_ocaml *)
type browser_file_id = {
  id: int;
  name: string;
}

let from_js_file_id (file_id : Js.Unsafe.any) (file_name : Js.Unsafe.any) : browser_file_id =
  let js_num = Js.Unsafe.coerce file_id in
  let js_str = Js.Unsafe.coerce file_name in
  { id = int_of_float (Js.to_float js_num); name = Js.to_string js_str }

let promise_string_to_lwt (js_promise : Js.Unsafe.any) : string Lwt.t =
  let result, wake = Lwt.wait () in
  let then_cb = Js.wrap_callback (fun (str : Js.js_string Js.t) ->
      debug_log "[alsdiff] Decompression then callback";
      (try Lwt.wakeup wake (Js.to_string str) with _ -> ());
      Js.undefined) in
  let catch_cb = Js.wrap_callback (fun (error : Js.Unsafe.any) ->
      debug_log "[alsdiff] Decompression catch callback";
      let error_msg =
        try
          let to_string_fn = Js.Unsafe.coerce error in
          Js.to_string (Js.Unsafe.call to_string_fn Js.undefined [||])
        with _ -> "Unknown decompression error"
      in
      (try Lwt.wakeup_exn wake (File_error ("", error_msg)) with _ -> ());
      Js.undefined) in
  let t = Js.Unsafe.meth_call js_promise "then" [| Js.Unsafe.inject then_cb |] in
  ignore (Js.Unsafe.meth_call t "catch" [| Js.Unsafe.inject catch_cb |]);
  result

let decompress_file_to_string (file_obj : Js.Unsafe.any) : string Lwt.t =
  let js_code =
    {js|(async function(f) {
      const dlog = (...args) => { if (globalThis.__alsdiff_debug) console.log(...args); };
      try {
        dlog('[alsdiff] Starting decompression with pako.js...');
        const buffer = await f.arrayBuffer();
        dlog('[alsdiff] Got ArrayBuffer, size:', buffer.byteLength);

        const compressed = new Uint8Array(buffer);
        const decompressed = pako.ungzip(compressed);
        dlog('[alsdiff] Decompressed with pako, size:', decompressed.length);

        const text = new TextDecoder().decode(decompressed);
        dlog('[alsdiff] Converted to text, length:', text.length);
        return text;
      } catch (e) {
        console.error('[alsdiff] Decompression error:', e);
        throw 'Decompression failed: ' + (e && e.message ? e.message : e);
      }
    })|js}
  in
  let js_fun = Js.Unsafe.js_expr js_code in
  let js_promise =
    Js.Unsafe.call js_fun Js.undefined [| Js.Unsafe.inject file_obj |]
  in
  promise_string_to_lwt js_promise

let decompress_file_id_to_string (file_id : int) (file_name : string) : string Lwt.t =
  let js_code =
    {js|(async function(fileId, fileName) {
      const dlog = (...args) => { if (globalThis.__alsdiff_debug) console.log(...args); };
      try {
        dlog('[alsdiff] Starting decompression with pako.js for:', fileName, 'ID:', fileId);
        const file = globalThis.__alsdiff_files && globalThis.__alsdiff_files[fileId];
        if (!file) {
          throw new Error('File not found in storage: ID ' + fileId);
        }

        const buffer = await file.arrayBuffer();
        dlog('[alsdiff] Got ArrayBuffer, size:', buffer.byteLength);

        const compressed = new Uint8Array(buffer);
        const decompressed = pako.ungzip(compressed);
        dlog('[alsdiff] Decompressed with pako, size:', decompressed.length);

        const text = new TextDecoder().decode(decompressed);
        dlog('[alsdiff] Converted to text, length:', text.length);
        return text;
      } catch (e) {
        console.error('[alsdiff] Decompression error:', e);
        throw 'Decompression failed: ' + (e && e.message ? e.message : e);
      }
    })|js}
  in
  let js_fun = Js.Unsafe.js_expr js_code in
  let js_promise =
    Js.Unsafe.call js_fun Js.undefined
      [| Js.Unsafe.inject (Js.float (float_of_int file_id));
         Js.Unsafe.inject (Js.string file_name) |]
  in
  promise_string_to_lwt js_promise

let has_pako () : bool =
  Js.to_bool (Js.Unsafe.js_expr "typeof pako !== 'undefined'")

let decompress_als_to_string (browser_file : browser_file) : string Lwt.t =
  let open Lwt.Syntax in
  if not (String.ends_with ~suffix:".als" browser_file.name) then
    Lwt.fail (File_error (browser_file.name, "Not an .als file"))
  else if not (has_pako ()) then
    Lwt.fail (File_error (browser_file.name, "pako.js library not loaded"))
  else
    let* result =
      Lwt.catch
        (fun () -> decompress_file_to_string browser_file.file_obj)
        (fun e -> Lwt.fail (File_error (browser_file.name, Printexc.to_string e)))
    in
    Lwt.return result

let open_als (browser_file : browser_file) : Alsdiff_base.Xml.t Lwt.t =
  let open Lwt.Syntax in
  let* contents = decompress_als_to_string browser_file in
  Lwt.catch
    (fun () -> Lwt.return (Alsdiff_base.Xml.read_string contents))
    (fun e -> Lwt.fail (File_error (browser_file.name, Printexc.to_string e)))

let decompress_file_id_als_to_string (browser_file : browser_file_id) : string Lwt.t =
  let open Lwt.Syntax in
  if not (String.ends_with ~suffix:".als" browser_file.name) then
    Lwt.fail (File_error (browser_file.name, "Not an .als file"))
  else if not (has_pako ()) then
    Lwt.fail (File_error (browser_file.name, "pako.js library not loaded"))
  else
    let* result =
      Lwt.catch
        (fun () -> decompress_file_id_to_string browser_file.id browser_file.name)
        (fun e -> Lwt.fail (File_error (browser_file.name, Printexc.to_string e)))
    in
    Lwt.return result

let open_als_by_id (browser_file : browser_file_id) : Alsdiff_base.Xml.t Lwt.t =
  let open Lwt.Syntax in
  let* contents = decompress_file_id_als_to_string browser_file in
  Lwt.catch
    (fun () -> Lwt.return (Alsdiff_base.Xml.read_string contents))
    (fun e -> Lwt.fail (File_error (browser_file.name, Printexc.to_string e)))

module Browser_file = struct
  type t = browser_file
  let from_js_file = from_js_file
  let from_js_blob = from_js_blob
end

module Browser_file_id = struct
  type t = browser_file_id
  let from_js_file_id = from_js_file_id
end

type output_mode = Tree | Stats | Json

type config = {
  output_mode: output_mode;
  renderer_config: Text_renderer.detail_config;
}

type preset =
  [ `Compact | `Composer | `Full | `Inline | `Mixing | `Quiet | `Verbose ]

type browser_options = {
  positional_args: string list;
  git_mode: bool;
  output_mode: output_mode;
  config_json: string option;
  preset: preset option;
  prefix_added: string option;
  prefix_removed: string option;
  prefix_modified: string option;
  prefix_unchanged: string option;
  note_name_style: View_model.note_display_style option;
  max_collection_items: int option;
  deprecated_fields: string list;
}

let default_config = {
  output_mode = Tree;
  renderer_config = Text_renderer.quiet;
}

let default_browser_options = {
  positional_args = [];
  git_mode = false;
  output_mode = Tree;
  config_json = None;
  preset = None;
  prefix_added = None;
  prefix_removed = None;
  prefix_modified = None;
  prefix_unchanged = None;
  note_name_style = None;
  max_collection_items = None;
  deprecated_fields = [];
}

let invalid_config_object_sentinel = "__alsdiff_invalid_config_object__"

let ( let* ) r f =
  match r with
  | Ok v -> f v
  | Error _ as e -> e

let parse_preset_args = function
  | `Compact -> Text_renderer.compact
  | `Composer -> Text_renderer.composer
  | `Full -> Text_renderer.full
  | `Inline -> Text_renderer.inline
  | `Mixing -> Text_renderer.mixing
  | `Quiet -> Text_renderer.quiet
  | `Verbose -> Text_renderer.verbose

let output_mode_of_string (s : string) : (output_mode, string) result =
  match String.lowercase_ascii (String.trim s) with
  | "tree" -> Ok Tree
  | "stats" -> Ok Stats
  | "json" -> Ok Json
  | _ -> Error "Invalid mode: expected 'tree', 'stats', or 'json'"

let preset_of_string (s : string) : (preset, string) result =
  match String.lowercase_ascii (String.trim s) with
  | "compact" -> Ok `Compact
  | "composer" -> Ok `Composer
  | "full" -> Ok `Full
  | "inline" -> Ok `Inline
  | "mixing" -> Ok `Mixing
  | "quiet" -> Ok `Quiet
  | "verbose" -> Ok `Verbose
  | _ -> Error "Invalid preset: expected one of compact|composer|full|inline|mixing|quiet|verbose"

let note_name_style_of_string (s : string) : (View_model.note_display_style, string) result =
  match String.lowercase_ascii (String.trim s) with
  | "sharp" -> Ok View_model.Sharp
  | "flat" -> Ok View_model.Flat
  | _ -> Error "Invalid noteNameStyle: expected 'Sharp' or 'Flat'"

let normalize_js_options_to_json_string (raw_options : Js.Unsafe.any) : string =
  let js_fun =
    Js.Unsafe.js_expr
      {js|((raw) => {
        const out = {
          deprecatedFields: [],
        };
        if (!raw || typeof raw !== "object") {
          return JSON.stringify(out);
        }

        const has = (k) => Object.prototype.hasOwnProperty.call(raw, k);
        const pick = (...keys) => {
          for (const key of keys) {
            if (has(key) && raw[key] !== undefined && raw[key] !== null) {
              return raw[key];
            }
          }
          return undefined;
        };
        const markDeprecated = (name, keys) => {
          for (const key of keys) {
            if (has(key)) {
              out.deprecatedFields.push(name);
              return;
            }
          }
        };

        const positionalArgs = pick("positionalArgs", "positional_args", "positional-args");
        if (Array.isArray(positionalArgs)) {
          out.positionalArgs = positionalArgs.map((x) => String(x));
        }

        const gitMode = pick("gitMode", "git_mode", "git-mode");
        if (gitMode !== undefined) {
          out.gitMode = !!gitMode;
        }

        const mode = pick("mode");
        if (mode !== undefined) {
          out.mode = String(mode);
        }

        const config = pick("config");
        if (config !== undefined) {
          if (typeof config === "string") {
            out.config = config;
          } else {
            try {
              out.config = JSON.stringify(config);
            } catch (_error) {
              out.config = "__alsdiff_invalid_config_object__";
            }
          }
        }

        const preset = pick("preset");
        if (preset !== undefined) {
          out.preset = String(preset);
        }

        const prefixAdded = pick("prefixAdded", "prefix_added", "prefix-added");
        if (prefixAdded !== undefined) {
          out.prefixAdded = String(prefixAdded);
        }
        const prefixRemoved = pick("prefixRemoved", "prefix_removed", "prefix-removed");
        if (prefixRemoved !== undefined) {
          out.prefixRemoved = String(prefixRemoved);
        }
        const prefixModified = pick("prefixModified", "prefix_modified", "prefix-modified");
        if (prefixModified !== undefined) {
          out.prefixModified = String(prefixModified);
        }
        const prefixUnchanged = pick("prefixUnchanged", "prefix_unchanged", "prefix-unchanged");
        if (prefixUnchanged !== undefined) {
          out.prefixUnchanged = String(prefixUnchanged);
        }

        const noteNameStyle = pick("noteNameStyle", "note_name_style", "note-name-style");
        if (noteNameStyle !== undefined) {
          out.noteNameStyle = String(noteNameStyle);
        }

        const maxCollectionItems = pick("maxCollectionItems", "max_collection_items", "max-collection-items");
        if (maxCollectionItems !== undefined && maxCollectionItems !== "") {
          out.maxCollectionItems = maxCollectionItems;
        }

        markDeprecated("dumpPreset", ["dumpPreset", "dump_preset", "dump-preset"]);
        markDeprecated("validateConfig", ["validateConfig", "validate_config", "validate-config"]);
        markDeprecated("dumpSchema", ["dumpSchema", "dump_schema", "dump-schema"]);

        out.deprecatedFields = Array.from(new Set(out.deprecatedFields));
        return JSON.stringify(out);
      })|js}
  in
  let js_str : Js.js_string Js.t =
    Js.Unsafe.coerce (Js.Unsafe.fun_call js_fun [| Js.Unsafe.inject raw_options |])
  in
  Js.to_string js_str

let parse_string_list = function
  | `List items ->
    List.fold_right
      (fun item acc ->
         let* values = acc in
         match item with
         | `String s -> Ok (s :: values)
         | _ -> Error "Invalid positionalArgs: expected array of strings")
      items
      (Ok [])
  | _ -> Error "Invalid positionalArgs: expected array of strings"

let parse_bool_value field_name = function
  | `Bool b -> Ok b
  | _ -> Error (Printf.sprintf "Invalid %s: expected boolean" field_name)

let parse_int_value field_name = function
  | `Int i -> Ok i
  | `Intlit s -> begin
      match int_of_string_opt s with
      | Some i -> Ok i
      | None -> Error (Printf.sprintf "Invalid %s: expected integer" field_name)
    end
  | `Float f when Float.floor f = f -> Ok (int_of_float f)
  | `String s -> begin
      match int_of_string_opt (String.trim s) with
      | Some i -> Ok i
      | None -> Error (Printf.sprintf "Invalid %s: expected integer" field_name)
    end
  | _ -> Error (Printf.sprintf "Invalid %s: expected integer" field_name)

let parse_browser_options (raw_options : Js.Unsafe.any) : (browser_options, string) result =
  let json_str = normalize_js_options_to_json_string raw_options in
  try
    let json = Yojson.Safe.from_string json_str in
    let fields =
      match json with
      | `Assoc pairs -> pairs
      | _ -> []
    in
    let find_field key = List.assoc_opt key fields in

    let* positional_args =
      match find_field "positionalArgs" with
      | None -> Ok default_browser_options.positional_args
      | Some value -> parse_string_list value
    in

    let* git_mode =
      match find_field "gitMode" with
      | None -> Ok default_browser_options.git_mode
      | Some value -> parse_bool_value "gitMode" value
    in

    let* output_mode =
      match find_field "mode" with
      | None -> Ok default_browser_options.output_mode
      | Some (`String s) -> output_mode_of_string s
      | Some _ -> Error "Invalid mode: expected string"
    in

    let* config_json =
      match find_field "config" with
      | None -> Ok default_browser_options.config_json
      | Some (`String s) ->
        if s = invalid_config_object_sentinel then
          Error "Invalid config: options.config must be JSON-serializable"
        else
          Ok (Some s)
      | Some _ -> Error "Invalid config: expected JSON string or JSON object"
    in

    let* preset =
      match find_field "preset" with
      | None -> Ok default_browser_options.preset
      | Some (`String s) ->
        let* p = preset_of_string s in
        Ok (Some p)
      | Some _ -> Error "Invalid preset: expected string"
    in

    let parse_optional_string field_name =
      match find_field field_name with
      | None -> Ok None
      | Some (`String s) -> Ok (Some s)
      | Some _ -> Error (Printf.sprintf "Invalid %s: expected string" field_name)
    in

    let* prefix_added = parse_optional_string "prefixAdded" in
    let* prefix_removed = parse_optional_string "prefixRemoved" in
    let* prefix_modified = parse_optional_string "prefixModified" in
    let* prefix_unchanged = parse_optional_string "prefixUnchanged" in

    let* note_name_style =
      match find_field "noteNameStyle" with
      | None -> Ok None
      | Some (`String s) ->
        let* style = note_name_style_of_string s in
        Ok (Some style)
      | Some _ -> Error "Invalid noteNameStyle: expected string"
    in

    let* max_collection_items =
      match find_field "maxCollectionItems" with
      | None -> Ok None
      | Some value ->
        let* parsed = parse_int_value "maxCollectionItems" value in
        Ok (Some parsed)
    in

    let* deprecated_fields =
      match find_field "deprecatedFields" with
      | None -> Ok []
      | Some value -> parse_string_list value
    in

    Ok {
      positional_args;
      git_mode;
      output_mode;
      config_json;
      preset;
      prefix_added;
      prefix_removed;
      prefix_modified;
      prefix_unchanged;
      note_name_style;
      max_collection_items;
      deprecated_fields;
    }
  with
  | Yojson.Json_error msg -> Error ("Invalid options payload: " ^ msg)

let parse_config_from_json_string (json_str : string) : (Text_renderer.detail_config, string) result =
  try
    let json_basic = Yojson.Basic.from_string json_str in
    match Text_renderer.validate_config_json json_basic with
    | Error err -> Error ("Config validation failed: " ^ err.details)
    | Ok () ->
      let filtered = Text_renderer.filter_schema_metadata_fields json_basic in
      let safe_json = Yojson.Safe.from_string (Yojson.Basic.to_string filtered) in
      begin
        match Text_renderer.detail_config_of_yojson_with_default safe_json with
        | Ok cfg -> Ok cfg
        | Error msg -> Error ("Config parsing failed: " ^ msg)
      end
  with
  | Yojson.Json_error msg -> Error ("Config JSON parse error: " ^ msg)

let build_base_renderer_config
    ~(default_config : Text_renderer.detail_config)
    (options : browser_options) : (Text_renderer.detail_config, string) result =
  match options.config_json with
  | Some json_str -> parse_config_from_json_string json_str
  | None ->
    begin
      match options.preset with
      | Some preset -> Ok (parse_preset_args preset)
      | None -> Ok default_config
    end

let stats_incompatible_flags_provided (options : browser_options) =
  options.prefix_added <> None
  || options.prefix_removed <> None
  || options.prefix_modified <> None
  || options.prefix_unchanged <> None
  || options.note_name_style <> None
  || options.max_collection_items <> None

let resolve_runtime_config_from_options (raw_options : Js.Unsafe.any) : (config, string) result =
  let* options = parse_browser_options raw_options in
  if options.deprecated_fields <> [] then
    debug_log
      (Printf.sprintf "[alsdiff] Ignored unsupported browser options: %s"
         (String.concat ", " options.deprecated_fields));
  let _ = options.positional_args in
  let _ = options.git_mode in
  match options.output_mode with
  | Stats ->
    if stats_incompatible_flags_provided options then
      Error "Error: --mode stats is incompatible with --prefix-*, --note-name-style, and --max-collection-items"
    else
      let* renderer_config =
        build_base_renderer_config ~default_config:Text_renderer.stats_default options
      in
      Ok { output_mode = Stats; renderer_config }
  | Json ->
    let* renderer_config =
      build_base_renderer_config ~default_config:Text_renderer.full options
    in
    Ok { output_mode = Json; renderer_config }
  | Tree ->
    let* base_renderer_config =
      build_base_renderer_config ~default_config:Text_renderer.quiet options
    in
    let renderer_config = {
      base_renderer_config with
      prefix_added =
        (match options.prefix_added with
         | Some s -> s
         | None -> base_renderer_config.prefix_added);
      prefix_removed =
        (match options.prefix_removed with
         | Some s -> s
         | None -> base_renderer_config.prefix_removed);
      prefix_modified =
        (match options.prefix_modified with
         | Some s -> s
         | None -> base_renderer_config.prefix_modified);
      prefix_unchanged =
        (match options.prefix_unchanged with
         | Some s -> s
         | None -> base_renderer_config.prefix_unchanged);
      note_name_style =
        (match options.note_name_style with
         | Some s -> s
         | None -> base_renderer_config.note_name_style);
      max_collection_items =
        (match options.max_collection_items with
         | Some n -> Some n
         | None -> base_renderer_config.max_collection_items);
    } in
    Ok { output_mode = Tree; renderer_config }

module Liveset_diff = struct
  let diff_livesets
      (file1 : Browser_file.t)
      (file2 : Browser_file.t)
      (config : config) : string Lwt.t =
    let open Lwt.Syntax in
    debug_log (Printf.sprintf "[alsdiff] diff_livesets start: %s vs %s" file1.name file2.name);
    let* xml1 = open_als file1 in
    debug_log "[alsdiff] diff_livesets loaded xml1";
    let* () = browser_yield () in
    let* xml2 = open_als file2 in
    debug_log "[alsdiff] diff_livesets loaded xml2";
    let* () = browser_yield () in
    let liveset1 = Liveset.create xml1 file1.name in
    debug_log "[alsdiff] diff_livesets created liveset1";
    let liveset2 = Liveset.create xml2 file2.name in
    debug_log "[alsdiff] diff_livesets created liveset2";
    let* () = browser_yield () in
    let patch = Liveset.diff liveset1 liveset2 in
    debug_log "[alsdiff] diff_livesets computed patch";
    let* () = browser_yield () in
    let views =
      [View_model.Item
         (View_model.create_liveset_item
            (if Liveset.Patch.is_empty patch then `Unchanged else `Modified patch))]
    in
    let output =
      match config.output_mode with
      | Tree -> Text_renderer.render config.renderer_config views
      | Stats -> Stats_renderer.render config.renderer_config views
      | Json -> Json_renderer.render config.renderer_config views
    in
    debug_log "[alsdiff] diff_livesets rendered output";
    Lwt.return output
end

module Liveset_diff_id = struct
  let diff_livesets_by_id
      (file1 : Browser_file_id.t)
      (file2 : Browser_file_id.t)
      (config : config) : string Lwt.t =
    let open Lwt.Syntax in
    debug_log
      (Printf.sprintf "[alsdiff] diff_livesets_by_id start: %s(%d) vs %s(%d)"
         file1.name file1.id file2.name file2.id);
    let* xml1 = open_als_by_id file1 in
    debug_log "[alsdiff] diff_livesets_by_id loaded xml1";
    let* () = browser_yield () in
    let* xml2 = open_als_by_id file2 in
    debug_log "[alsdiff] diff_livesets_by_id loaded xml2";
    let* () = browser_yield () in
    let liveset1 = Liveset.create xml1 file1.name in
    debug_log "[alsdiff] diff_livesets_by_id created liveset1";
    let liveset2 = Liveset.create xml2 file2.name in
    debug_log "[alsdiff] diff_livesets_by_id created liveset2";
    let* () = browser_yield () in
    let patch = Liveset.diff liveset1 liveset2 in
    debug_log "[alsdiff] diff_livesets_by_id computed patch";
    let* () = browser_yield () in
    let views =
      [View_model.Item
         (View_model.create_liveset_item
            (if Liveset.Patch.is_empty patch then `Unchanged else `Modified patch))]
    in
    let output =
      match config.output_mode with
      | Tree -> Text_renderer.render config.renderer_config views
      | Stats -> Stats_renderer.render config.renderer_config views
      | Json -> Json_renderer.render config.renderer_config views
    in
    debug_log "[alsdiff] diff_livesets_by_id rendered output";
    Lwt.return output
end

let create_js_promise_for_request (request_id : int) : Js.Unsafe.any =
  let js_fun =
    Js.Unsafe.js_expr
      {js|((id) => new Promise((resolve, reject) => {
        const key = String(id);
        if (!globalThis.__alsdiff_promises) {
          globalThis.__alsdiff_promises = Object.create(null);
        }
        globalThis.__alsdiff_promises[key] = {
          resolve,
          reject,
          settled: false,
        };
      }))|js}
  in
  Js.Unsafe.fun_call js_fun [| Js.Unsafe.inject (Js.number_of_float (float_of_int request_id)) |]

let settle_js_promise ~(request_id : int) ~(method_name : string) (payload : string) : unit =
  let js_fun =
    Js.Unsafe.js_expr
      {js|((id, methodName, payload) => {
        const key = String(id);
        const table = globalThis.__alsdiff_promises;
        if (!table || !table[key]) {
          return false;
        }
        const entry = table[key];
        if (entry.settled) {
          return false;
        }
        entry.settled = true;
        try {
          entry[methodName](payload);
        } finally {
          delete table[key];
        }
        return true;
      })|js}
  in
  ignore
  @@ Js.Unsafe.fun_call js_fun
    [| Js.Unsafe.inject (Js.number_of_float (float_of_int request_id));
       Js.Unsafe.inject (Js.string method_name);
       Js.Unsafe.inject (Js.string payload) |]

let next_request_id =
  let counter = ref 0 in
  fun () ->
    let id = !counter in
    incr counter;
    id

let lwt_to_js_promise (lwt : string Lwt.t) : Js.Unsafe.any =
  let request_id = next_request_id () in
  debug_log (Printf.sprintf "[alsdiff] lwt_to_js_promise called id=%d" request_id);
  let js_promise = create_js_promise_for_request request_id in
  Lwt.on_success lwt (fun result ->
      settle_js_promise ~request_id ~method_name:"resolve" result);
  Lwt.on_failure lwt (fun exn ->
      settle_js_promise ~request_id ~method_name:"reject" (Printexc.to_string exn));
  js_promise

let rejected_promise (message : string) : Js.Unsafe.any =
  lwt_to_js_promise (Lwt.fail (Failure message))

let diff_files_export
    (file1 : Js.Unsafe.any)
    (file2 : Js.Unsafe.any)
    (options : Js.Unsafe.any) : Js.Unsafe.any =
  debug_log "[alsdiff] diff_files_export called";
  let f1 = Browser_file.from_js_file file1 in
  let f2 = Browser_file.from_js_file file2 in
  match resolve_runtime_config_from_options options with
  | Error msg -> rejected_promise msg
  | Ok runtime_config ->
    let lwt_result = Liveset_diff.diff_livesets f1 f2 runtime_config in
    lwt_to_js_promise lwt_result

let diff_blobs_export
    (blob1 : Js.Unsafe.any)
    (name1 : Js.Unsafe.any)
    (blob2 : Js.Unsafe.any)
    (name2 : Js.Unsafe.any)
    (options : Js.Unsafe.any) : Js.Unsafe.any =
  debug_log "[alsdiff] diff_blobs_export called";
  let f1 = Browser_file.from_js_blob blob1 (Js.to_string (Js.Unsafe.coerce name1)) in
  let f2 = Browser_file.from_js_blob blob2 (Js.to_string (Js.Unsafe.coerce name2)) in
  match resolve_runtime_config_from_options options with
  | Error msg -> rejected_promise msg
  | Ok runtime_config ->
    let lwt_result = Liveset_diff.diff_livesets f1 f2 runtime_config in
    lwt_to_js_promise lwt_result

let diff_files_by_id_export
    (id1 : Js.Unsafe.any)
    (id2 : Js.Unsafe.any)
    (name1 : Js.Unsafe.any)
    (name2 : Js.Unsafe.any)
    (options : Js.Unsafe.any) : Js.Unsafe.any =
  debug_log "[alsdiff] diff_files_by_id_export called";
  let f1 = Browser_file_id.from_js_file_id id1 name1 in
  let f2 = Browser_file_id.from_js_file_id id2 name2 in
  match resolve_runtime_config_from_options options with
  | Error msg -> rejected_promise msg
  | Ok runtime_config ->
    let lwt_result = Liveset_diff_id.diff_livesets_by_id f1 f2 runtime_config in
    lwt_to_js_promise lwt_result

let set_debug_export (enabled : Js.Unsafe.any) : unit =
  let js_bool : bool Js.t = Js.Unsafe.coerce enabled in
  debug_enabled := Js.to_bool js_bool;
  set_global_debug_flag !debug_enabled;
  if !debug_enabled then debug_log "[alsdiff] Debug logging enabled"

let () =
  set_global_debug_flag false;

  let obj =
    match Js.Opt.to_option (Js.Unsafe.get Js.Unsafe.global (Js.string "alsdiff")) with
    | Some existing -> Js.Unsafe.coerce existing
    | None -> Js.Unsafe.js_expr "({})"
  in

  Js.Unsafe.set obj (Js.string "diffFiles") (Js.Unsafe.inject diff_files_export);
  Js.Unsafe.set obj (Js.string "diffBlobs") (Js.Unsafe.inject diff_blobs_export);
  Js.Unsafe.set obj (Js.string "diffFilesById") (Js.Unsafe.inject diff_files_by_id_export);
  Js.Unsafe.set obj (Js.string "setDebug") (Js.Unsafe.inject set_debug_export);

  Js.Unsafe.set Js.Unsafe.global (Js.string "alsdiff") obj
