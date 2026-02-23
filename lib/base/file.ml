(** [File_error filename msg] raised when file operations fail. *)
exception File_error of string * string

(** Decompress the .als file with [filename] and return its contents as a string. *)
let decompress_als_to_string filename =
  try
    Base_io.decompress_als_to_string filename
  with
  | Base_io.File_error (file, msg) ->
    raise (File_error (file, msg))
  | e ->
    raise (File_error (filename, Printexc.to_string e))

let decompress_als filename =
  let basename = Filename.basename filename |> Filename.remove_extension in
  let temp_file = Filename.temp_file basename ".xml" in
  let out_chan = open_out temp_file in
  try
    output_string out_chan (decompress_als_to_string filename);
    close_out out_chan;
    temp_file
  with
  | e ->
    close_out_noerr out_chan;
    (try Sys.remove temp_file with _ -> ());
    raise e

(** open the .als file with [filename], and return the parsed XML tree. *)
let open_als filename =
  try
    filename
    |> decompress_als_to_string
    |> Xml.read_string
  with
  | Xmlm.Error ((line, col), err) ->
    let msg = Printf.sprintf "XML parsing error at line %d, col %d: %s" line col (Xmlm.error_message err) in
    raise (File_error (filename, msg))
  | File_error _ as e -> raise e
  | e ->
    raise (File_error (filename, "Failed to open ALS file: " ^ Printexc.to_string e))

let time_it (f : unit -> 'a)  =
  let start_time = Sys.time() in
  let result = f () in
  let end_time = Sys.time() in
  Printf.printf "Execute time: %f seconds\n" (end_time -. start_time);
  result
