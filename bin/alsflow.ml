open Alsdiff_base
open Alsdiff_live
open Alsdiff_output
open Cmdliner
open Cmdliner.Term.Syntax

type config = {
  file : string;
  format : string;
  direction : string;
  include_external : bool;
  include_routing : bool;
  include_sends : bool;
}

let alsflow_cmd ~config ~domain_mgr : int =
  let xml = File.open_als config.file in
  let liveset =
    Eio.Domain_manager.run domain_mgr @@ fun () ->
    Liveset.create xml config.file
  in
  let options : Flowchart.options = {
    direction = config.direction;
    include_external = config.include_external;
    include_routing = config.include_routing;
    include_sends = config.include_sends;
    use_subgraph_id_for_groups =
      (match config.format with
       | "mermaid" -> true
       | "dot" -> false
       | _ -> true);
  } in
  let output =
    match config.format with
    | "mermaid" -> Mermaid_renderer.render_flowchart ~xml ~liveset ~options
    | "dot" -> Dot_renderer.render_flowchart ~xml ~liveset ~options
    | _ -> failwith "Unknown format"
  in
  Fmt.pr "%s" output;
  0

let file_arg =
  let doc = "ALS file to render as a routing flowchart." in
  Arg.(required & pos 0 (some string) None & info [] ~docv:"FILE.als" ~doc)

let format_arg =
  let doc = "Output format: mermaid or dot." in
  Arg.(value & opt (enum ["mermaid", "mermaid"; "dot", "dot"]) "mermaid" & info ["format"] ~docv:"FORMAT" ~doc)

let direction_arg =
  let doc = "Flowchart direction: LR (left-right) or TD (top-down)." in
  Arg.(value & opt (enum ["LR", "LR"; "TD", "TD"]) "TD" & info ["direction"] ~docv:"DIR" ~doc)

let include_external_arg =
  let doc = "Include external routing nodes." in
  Arg.(value & opt bool true & info ["include-external"] ~doc)

let include_routing_arg =
  let doc = "Include routing edges." in
  Arg.(value & opt bool true & info ["include-routing"] ~doc)

let include_sends_arg =
  let doc = "Include send edges." in
  Arg.(value & opt bool true & info ["include-sends"] ~doc)

let cmd =
  let+ file = file_arg
  and+ format = format_arg
  and+ direction = direction_arg
  and+ include_external = include_external_arg
  and+ include_routing = include_routing_arg
  and+ include_sends = include_sends_arg in
  let config = { file; format; direction; include_external; include_routing; include_sends } in
  Eio_main.run @@ fun env ->
  let domain_mgr = Eio.Stdenv.domain_mgr env in
  alsflow_cmd ~config ~domain_mgr

let () =
  let info =
    Cmd.info "alsflow" ~doc:"Render ALS track routing as a flowchart (Mermaid or DOT format)"
  in
  exit (Cmd.eval' (Cmd.v info cmd))
