open Cmdliner
open Cmdliner.Term.Syntax

let merge_cmd base_file ours_file theirs_file =
  let result =
    Alsdiff_merge.Merge.three_way_merge
      ~base_file ~ours_file ~theirs_file
  in
  Alsdiff_base.File.write_als ours_file result.xml;
  List.iter (fun c ->
      Fmt.epr "%a@." Alsdiff_merge.Conflict.pp c
    ) result.conflicts;
  if result.clean then 0 else 1

let base_file =
  let doc = "Base (ancestor) .als file" in
  Arg.(required & pos 0 (some string) None & info [] ~docv:"BASE" ~doc)

let ours_file =
  let doc = "Ours (current) .als file — also used as output" in
  Arg.(required & pos 1 (some string) None & info [] ~docv:"OURS" ~doc)

let theirs_file =
  let doc = "Theirs (other branch) .als file" in
  Arg.(required & pos 2 (some string) None & info [] ~docv:"THEIRS" ~doc)

let cmd =
  let doc = "Three-way merge driver for Ableton Live Set (.als) files" in
  let man = [
    `S Manpage.s_description;
    `P "$(cmd) performs a three-way merge of Ableton Live Set files, designed for use as a git merge driver.";
    `P "Exit code 0 = clean merge, 1 = conflicts (best-effort merge written), 2 = error.";
    `S "GIT MERGE DRIVER";
    `P "Configure git (.gitconfig):";
    `Pre "[merge \"als\"]";
    `Pre "    name = ALS merge driver";
    `Pre "    driver = alsmerge %O %A %B";
    `P "Configure .gitattributes:";
    `Pre "*.als merge=als";
    `S Manpage.s_examples;
    `P "Manual merge:";
    `Pre "$(cmd) base.als ours.als theirs.als";
    `P "Git merge driver (automatic):";
    `Pre "git merge feature-branch";
  ] in
  let exits =
    Cmd.Exit.info 0 ~doc:"clean merge, no conflicts" ::
    Cmd.Exit.info 1 ~doc:"merge completed with conflicts" ::
    Cmd.Exit.info 2 ~doc:"error" ::
    []
  in
  Cmd.make (Cmd.info "alsmerge" ~doc ~man ~exits) @@
  let+ base_file and+ ours_file and+ theirs_file in
  try merge_cmd base_file ours_file theirs_file
  with
  | Alsdiff_base.File.File_error (file, msg) ->
    Fmt.epr "Error: Failed to process file '%s': %s@." file msg;
    2
  | Alsdiff_base.Xml.Xml_error (xml, msg) ->
    Fmt.epr "Error: Invalid XML: %s@.%a@." msg Alsdiff_base.Xml.pp xml;
    2
  | Sys_error msg ->
    Fmt.epr "System error: %s@." msg;
    2
  | Failure msg ->
    Fmt.epr "Error: %s@." msg;
    2
  | exn ->
    Fmt.epr "Unexpected error: %s@.%s@."
      (Printexc.to_string exn) (Printexc.get_backtrace ());
    2

let () =
  Printexc.record_backtrace true;
  if !Sys.interactive then () else
    exit (Cmd.eval' cmd)
