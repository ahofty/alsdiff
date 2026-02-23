(** Main entry point for browser compilation

    This file ensures the browser library is included in the compiled JavaScript.
    The library initialization code exports functions to window.alsdiff
*)

(* Force inclusion of the browser library module to prevent dead code elimination *)
let _ = Alsdiff_browser.diff_files_export

(* The library's initialization code (let () =) will run and create window.alsdiff *)
