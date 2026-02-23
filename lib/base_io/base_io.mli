(** Cross-platform I/O operations for reading Ableton Live Set files *)

exception File_error of string * string
(** [File_error (filename, msg)] raised when file operations fail *)

(** Decompress the .als file with [filename] and return its contents as a string *)
val decompress_als_to_string : string -> string
