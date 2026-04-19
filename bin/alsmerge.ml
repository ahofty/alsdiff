open Cmdliner
open Cmdliner.Term.Syntax

let merge_cmd use_tui base_file ours_file theirs_file =
  let ctx = Alsdiff_merge.Merge.three_way_merge_context
      ~base_file ~ours_file ~theirs_file
  in
  if ctx.clean then begin
    let merged_xml = Alsdiff_merge.Xml_merge.apply_merge
        ~base_xml:ctx.base_xml ~base:ctx.base_ls ~ours:ctx.ours_ls ~theirs:ctx.theirs_ls
        ~action:ctx.action ~track_merges:ctx.track_merges
        ~return_merges:ctx.return_merges ~locator_merges:ctx.locator_merges
        ~ours_patch:ctx.ours_patch ~theirs_patch:ctx.theirs_patch ()
    in
    Alsdiff_base.File.write_als ours_file merged_xml;
    0
  end else if use_tui then begin
    let exit_code = Alsmerge_tui_lib.App.run
        ~context:ctx ~ours_file ~base_file ~theirs_file ()
    in
    exit_code
  end else begin
    let merged_xml = Alsdiff_merge.Xml_merge.apply_merge
        ~base_xml:ctx.base_xml ~base:ctx.base_ls ~ours:ctx.ours_ls ~theirs:ctx.theirs_ls
        ~action:ctx.action ~track_merges:ctx.track_merges
        ~return_merges:ctx.return_merges ~locator_merges:ctx.locator_merges
        ~ours_patch:ctx.ours_patch ~theirs_patch:ctx.theirs_patch ()
    in
    Alsdiff_base.File.write_als ours_file merged_xml;
    List.iter (fun c ->
        Fmt.epr "%a@." Alsdiff_merge.Conflict.pp c
      ) ctx.conflicts;
    1
  end

let use_tui =
  let doc = "Launch interactive TUI when conflicts are detected" in
  Arg.(value & flag & info ["tui"; "t"] ~doc)

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
    `Pre "    driver = alsmerge --tui %O %A %B";
    `P "Configure .gitattributes:";
    `Pre "*.als merge=als";
    `S Manpage.s_examples;
    `P "Manual merge (CLI only):";
    `Pre "$(cmd) base.als ours.als theirs.als";
    `P "Interactive merge with TUI:";
    `Pre "$(cmd) --tui base.als ours.als theirs.als";
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
  let+ use_tui and+ base_file and+ ours_file and+ theirs_file in
  try merge_cmd use_tui base_file ours_file theirs_file
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
