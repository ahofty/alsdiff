(** [File_error filename msg] raised when file operations fail. *)
exception File_error of string * string

(** Estimate buffer size based on compressed file size and typical compression ratios.
    Ableton Live sets typically compress around 10-20:1, so we use a conservative
    15:1 estimate with minimum buffer sizes to handle small files efficiently.
*)
let estimate_buffer_size compressed_size =
  let min_buffer = 65536 in (* 64KB minimum for small files *)
  let estimated_expansion = 15 in (* Conservative 15:1 compression ratio estimate *)
  let estimated_size = compressed_size * estimated_expansion in
  max min_buffer (min estimated_size (16*1024*1024)) (* Cap at 16MB for typical Ableton projects *)

(** Decompress the .als file with [filename] and return its contents as a string. *)
let decompress_als_to_string filename =
  try
    let compressed_size = Unix.stat filename |> (fun st -> st.st_size) in
    let buffer = Buffer.create (estimate_buffer_size compressed_size) in
    let gz_in = Gzip.open_in filename in
    let chunk = Bytes.create 8192 in
    let rec copy_loop () =
      let bytes_read = Gzip.input gz_in chunk 0 (Bytes.length chunk) in
      if bytes_read > 0 then (
        Buffer.add_subbytes buffer chunk 0 bytes_read;
        copy_loop ()
      )
    in
    (try copy_loop () with End_of_file -> ());
    Gzip.close_in gz_in;
    Buffer.contents buffer
  with
  | Unix.Unix_error (err, func, _) ->
    let msg = Printf.sprintf "%s: %s" func (Unix.error_message err) in
    raise (File_error (filename, msg))
  | Sys_error msg ->
    raise (File_error (filename, msg))
  | Gzip.Error msg ->
    raise (File_error (filename, "Gzip error: " ^ msg))
  | e ->
    raise (File_error (filename, "Unexpected error: " ^ Printexc.to_string e))
