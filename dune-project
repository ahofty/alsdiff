(lang dune 3.19)

(name alsdiff)

(version 0.2.0)

(source
 (github krfantasy/alsdiff))

(authors "Jay Xu <jay.xu.krfantasy@gmail.com>")

(maintainers "Jay Xu <jay.xu.krfantasy@gmail.com>")

(license THE-LICENSE.txt)

(documentation https://github.com/krfantasy/alsdiff)

(package
 (name alsdiff)
 (synopsis "diff tool for Ableton Live Set(.als)")
 (description "diff tool for Ableton Live Set(.als)")
 (depends
   (ocaml (>= 5.3.0))
    re
    xmlm
    angstrom
   (cmdliner (>= 2.0.0))
    dune-build-info
   (ocp-indent :build)
    alcotest
    ppx_deriving
    ppx_deriving_yojson
    ppx_deriving_jsonschema
    jsonschema
    fmt
    ptime

   ; Native-specific dependencies (default profile)
   camlzip
   eio
   eio_main

   ; JS-specific dependencies (opt-in via OPAMVAR_with_js=true)
   (lwt :with_js)
   (lwt_ppx :with_js)
   (js_of_ocaml :with_js)
   (js_of_ocaml-ppx :with_js))
 (tags
  ("ableton live" "git" "version control")))

(generate_opam_files true)

(formatting disabled)

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/reference/dune-project/index.html
