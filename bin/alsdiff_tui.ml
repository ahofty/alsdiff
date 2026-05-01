open Alsdiff_base
open Alsdiff_live
open Alsdiff_output
open Eio.Std
open View_model
open Cmdliner
open Cmdliner.Term.Syntax

let load_liveset ~domain_mgr file =
  Eio.Domain_manager.run domain_mgr @@ fun () ->
  let xml = File.open_als file in
  Liveset.create xml file

let create_views ~note_name_style ~(format_time : float -> View_model.field_value) (change : (Liveset.t, Liveset.Patch.t) Diff.structured_change)
  : View_model.view list =
  let item = View_model.create_liveset_item ~note_name_style ~format_time change in
  [View_model.Item item]

type config = {
  positional_args: string list;
  note_name_style: note_display_style;
  time_format: time_format;
}

let tui_cmd ~config ~domain_mgr : int =
  match config.positional_args with
  | [] ->
    Alsdiff_tui_lib.App.run_browser
      ~root:(Sys.getcwd ())
      ~note_name_style:config.note_name_style ();
    0
  | [f1; f2] ->
    let liveset1, liveset2 = Fiber.pair
        (fun () -> load_liveset ~domain_mgr f1)
        (fun () -> load_liveset ~domain_mgr f2)
    in

    let liveset_patch = Liveset.diff liveset1 liveset2 in
    let has_changes = not (Liveset.Patch.is_empty liveset_patch) in

    let liveset_change =
      if has_changes then
        `Modified liveset_patch
      else
        `Unchanged
    in

    let format_time = match config.time_format with
      | QuarterNotes -> View_model.default_format_time
      | _ ->
        let main_track = match liveset2.Liveset.main with Track.Main m -> m | _ -> failwith "Liveset.main must be Track.Main" in
        View_model.make_format_time config.time_format
          ~tempo_events:(Track.MainTrack.get_tempo_events main_track)
          ~ts_events:(Track.MainTrack.get_time_signature_events main_track)
          ()
    in
    let views = create_views ~note_name_style:config.note_name_style ~format_time liveset_change in

    (* Run the TUI *)
    Alsdiff_tui_lib.App.run ~views ~detail_config:Config.full
      ~time_format:config.time_format ();

    (* Print export output if any *)
    (match !Alsdiff_tui_lib.Update.export_output_ref with
     | Some output -> print_endline output; flush stdout
     | None -> ());

    0
  | _ ->
    assert false (* validated by Term.ret *)

let positional_args =
  let doc = "FILE1.als FILE2.als - the two Ableton Live Set files to compare" in
  Arg.(value & pos_all string [] & info [] ~docv:"FILES" ~doc)

let note_name_style =
  let doc = "Note name display style (Sharp or Flat)" in
  Arg.(value & opt (some (enum ["Sharp", Sharp; "Flat", Flat])) None & info ["note-name-style"] ~docv:"STYLE" ~doc)

let time_format =
  let doc = "Time format for time fields (QuarterNotes, BeatTime, RealTime)" in
  Arg.(value & opt (enum ["QuarterNotes", QuarterNotes; "BeatTime", BeatTime; "RealTime", RealTime]) QuarterNotes & info ["time-format"] ~docv:"FORMAT" ~doc)

let cmd =
  let doc = "Compare two Ableton Live Set (.als) files in an interactive terminal UI" in
  let man = [
    `S Manpage.s_description;
    `P "$(cmd) compares two Ableton Live Set files and displays the differences in an interactive terminal UI.";
    `P "The TUI provides keyboard navigation, detail level switching, search, and filtering capabilities.";
    `S Manpage.s_options;
    `P "$(b,--note-name-style STYLE) sets note name style to $(b,Sharp) or $(b,Flat).";
    `S "KEYBOARD SHORTCUTS";
    `P "Arrow keys or hjkl - Navigate through items";
    `P "Space - Expand/collapse items";
    `P "d - Cycle detail mode";
    `P "/ - Start search";
    `P "f - Toggle change filter";
    `P "n/p - Next/previous page";
    `P "q - Quit";
    `S Manpage.s_examples;
    `P "Compare two files:";
    `Pre "$(cmd) v1.als v2.als";
    `P "Compare with flat note names:";
    `Pre "$(cmd) v1.als v2.als --note-name-style Flat";
    `S Manpage.s_bugs;
    `P "Report bugs at https://github.com/krfantasy/alsdiff/issues";
  ] in
  let exits = Cmd.Exit.info 0 ~doc:"success" :: List.filter (fun e -> Cmd.Exit.info_code e <> 0) Cmd.Exit.defaults in
  Cmd.make (Cmd.info "alsdiff-tui" ~doc ~man ~exits) @@
  Term.ret @@
  let+ positional_args and+ note_name_style and+ time_format in
  let note_name_style = match note_name_style with
    | Some style -> style
    | None -> View_model.Sharp
  in
  let cfg = { positional_args; note_name_style; time_format } in
  let n = List.length positional_args in
  if n <> 0 && n <> 2 then
    `Error (true, "expected 0 or 2 file arguments")
  else
    `Ok cfg

let main () =
  Printexc.record_backtrace true;

  try
    match Cmd.eval_value ~catch:false cmd with
    | Ok (`Ok cfg) ->
      Eio_main.run @@ fun env ->
      let domain_mgr = Eio.Stdenv.domain_mgr env in
      tui_cmd ~config:cfg ~domain_mgr
    | Ok (`Version | `Help) -> 0
    | Error `Parse | Error `Term -> 1
    | Error `Exn -> 1
  with
  | File.File_error (file, msg) ->
    let bt = Printexc.get_backtrace () in
    Fmt.epr "Error: Failed to process file '%s': %s@.%s@." file msg bt;
    1
  | Xml.Xml_error (xml, msg) ->
    let bt = Printexc.get_backtrace () in
    Fmt.epr "Error: Invalid XML format: %s@.%a@.%s@." msg Xml.pp xml bt;
    1
  | Sys_error msg ->
    let bt = Printexc.get_backtrace () in
    Fmt.epr "System Error: %s@.%s@." msg bt;
    1
  | Failure msg ->
    let bt = Printexc.get_backtrace () in
    Fmt.epr "Error: %s@.%s@." msg bt;
    1
  | exn ->
    let bt = Printexc.get_backtrace () in
    Fmt.epr "Unexpected error: %s@.%s@." (Printexc.to_string exn) bt;
    1

let () =
  if !Sys.interactive then () else exit (main ())
