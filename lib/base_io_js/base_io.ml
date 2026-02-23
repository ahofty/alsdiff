exception File_error of string * string

(** Implementation for js_of_ocaml targeting Node.js environment.
    Uses Node.js fs and zlib modules for file I/O and decompression. *)
let decompress_als_to_string filename =
  (* Use Node.js to read gzipped file and decompress it in one call *)
  let js_code = "(function(path) { \
                 const fs = require('fs'); \
                 const zlib = require('zlib'); \
                 const compressed = fs.readFileSync(path); \
                 const decompressed = zlib.gunzipSync(compressed); \
                 return decompressed.toString('utf8'); \
                 })" in
  let js_result = Js_of_ocaml.Js.Unsafe.fun_call
      (Js_of_ocaml.Js.Unsafe.js_expr js_code)
      [| (Js_of_ocaml.Js.string filename |> Js_of_ocaml.Js.Unsafe.inject) |]
  in
  Js_of_ocaml.Js.to_string js_result
