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

   ; Native-specific dependencies
   (camlzip :with-native)
   (eio :with-native)
   (eio_main :with-native)

   ; JS-specific dependencies (for browser builds)
   lwt
   lwt_ppx
   js_of_ocaml
   js_of_ocaml-ppx)
 (tags
  ("ableton live" "git" "version control")))

(generate_opam_files true)

(formatting disabled)

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/reference/dune-project/index.html
