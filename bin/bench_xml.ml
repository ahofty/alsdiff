open Alsdiff_base
open Cmdliner.Term.Syntax

(* --- Mode --- *)

type mode = Dom | Streaming

(* --- Category --- *)

type category =
  | LivesetRoot
  | TrackNames
  | TrackStructure
  | MixerParams
  | Routing
  | Automation
  | DeviceCommon
  | PluginM4LGroup
  | Clip
  | Locator

let category_to_string = function
  | LivesetRoot -> "LivesetRoot"
  | TrackNames -> "TrackNames"
  | TrackStructure -> "TrackStructure"
  | MixerParams -> "MixerParams"
  | Routing -> "Routing"
  | Automation -> "Automation"
  | DeviceCommon -> "DeviceCommon"
  | PluginM4LGroup -> "PluginM4LGroup"
  | Clip -> "Clip"
  | Locator -> "Locator"

(* --- Query definition --- *)

type bench_query = {
  id : int;
  category : category;
  name : string;
  path : string;
}

(* --- 74 queries extracted from lib/live/*.ml create functions --- *)

let queries : bench_query list =
  [ (* A. Liveset Root — from liveset.ml *)
    { id = 0; category = LivesetRoot; name = "LiveSet"; path = "LiveSet" }
  ; { id = 1; category = LivesetRoot; name = "Tracks"; path = "/LiveSet/Tracks" }
  ; { id = 2; category = LivesetRoot; name = "MainTrack"; path = "/LiveSet/MainTrack" }
  ; { id = 3; category = LivesetRoot; name = "Locators"; path = "/LiveSet/Locators/Locators" }
  (* B. Track Names — from track.ml get_attr "/Name/EffectiveName" *)
  ; { id = 4; category = TrackNames; name = "MidiTrack-Name"
    ; path = "/LiveSet/Tracks/MidiTrack/Name/EffectiveName" }
  ; { id = 5; category = TrackNames; name = "AudioTrack-Name"
    ; path = "/LiveSet/Tracks/AudioTrack/Name/EffectiveName" }
  ; { id = 6; category = TrackNames; name = "GroupTrack-Name"
    ; path = "/LiveSet/Tracks/GroupTrack/Name/EffectiveName" }
  ; { id = 7; category = TrackNames; name = "ReturnTrack-Name"
    ; path = "/LiveSet/Tracks/ReturnTrack/Name/EffectiveName" }
  ; { id = 8; category = TrackNames; name = "MainTrack-Name"
    ; path = "/LiveSet/MainTrack/Name/EffectiveName" }
  (* C. Track Structure — from track.ml MidiTrack/AudioTrack/MainTrack.create *)
  ; { id = 9; category = TrackStructure; name = "AutomationEnvelope"
    ; path = "/LiveSet/Tracks/*/AutomationEnvelopes/*/AutomationEnvelope" }
  ; { id = 10; category = TrackStructure; name = "Devices"
    ; path = "/LiveSet/Tracks/*/DeviceChain/*/Devices" }
  ; { id = 11; category = TrackStructure; name = "Mixer"
    ; path = "/LiveSet/Tracks/*/DeviceChain/Mixer" }
  ; { id = 12; category = TrackStructure; name = "DeviceChain"
    ; path = "/LiveSet/Tracks/*/DeviceChain" }
  ; { id = 13; category = TrackStructure; name = "ArrangerMidiClip"
    ; path = "/**/ClipTimeable/ArrangerAutomation/Events/MidiClip" }
  ; { id = 14; category = TrackStructure; name = "AudioClip"
    ; path = "/**/AudioClip" }
  (* D. Mixer Parameters — from track.ml Mixer.create + MainMixer.create *)
  ; { id = 15; category = MixerParams; name = "Track-Volume"
    ; path = "/LiveSet/Tracks/*/DeviceChain/Mixer/Volume" }
  ; { id = 16; category = MixerParams; name = "Track-Volume-Manual"
    ; path = "/LiveSet/Tracks/*/DeviceChain/Mixer/Volume/Manual" }
  ; { id = 17; category = MixerParams; name = "Track-Pan"
    ; path = "/LiveSet/Tracks/*/DeviceChain/Mixer/Pan" }
  ; { id = 18; category = MixerParams; name = "Track-Pan-Manual"
    ; path = "/LiveSet/Tracks/*/DeviceChain/Mixer/Pan/Manual" }
  ; { id = 19; category = MixerParams; name = "Track-On"
    ; path = "/LiveSet/Tracks/*/DeviceChain/Mixer/On" }
  ; { id = 20; category = MixerParams; name = "Track-SoloSink"
    ; path = "/LiveSet/Tracks/*/DeviceChain/Mixer/SoloSink" }
  ; { id = 21; category = MixerParams; name = "Track-TrackSendHolder"
    ; path = "/LiveSet/Tracks/*/DeviceChain/Mixer/Sends/TrackSendHolder" }
  ; { id = 22; category = MixerParams; name = "Track-Send"
    ; path = "/LiveSet/Tracks/*/DeviceChain/Mixer/Sends/TrackSendHolder/Send" }
  ; { id = 23; category = MixerParams; name = "Track-HeadKeyMidi"
    ; path = "/LiveSet/Tracks/*/DeviceChain/Mixer/HeadKeyMidi" }
  ; { id = 24; category = MixerParams; name = "Main-Tempo"
    ; path = "/LiveSet/MainTrack/DeviceChain/Mixer/Tempo" }
  ; { id = 25; category = MixerParams; name = "Main-Tempo-Manual"
    ; path = "/LiveSet/MainTrack/DeviceChain/Mixer/Tempo/Manual" }
  ; { id = 26; category = MixerParams; name = "Main-TimeSignature"
    ; path = "/LiveSet/MainTrack/DeviceChain/Mixer/TimeSignature" }
  ; { id = 27; category = MixerParams; name = "Main-CrossFade"
    ; path = "/LiveSet/MainTrack/DeviceChain/Mixer/CrossFade" }
  ; { id = 28; category = MixerParams; name = "Main-GlobalGrooveAmount"
    ; path = "/LiveSet/MainTrack/DeviceChain/Mixer/GlobalGrooveAmount" }
  (* E. Routing — from track.ml RoutingSet.create + Routing.create *)
  ; { id = 29; category = Routing; name = "AudioInputRouting"
    ; path = "/LiveSet/Tracks/*/DeviceChain/AudioInputRouting" }
  ; { id = 30; category = Routing; name = "AudioOutputRouting"
    ; path = "/LiveSet/Tracks/*/DeviceChain/AudioOutputRouting" }
  ; { id = 31; category = Routing; name = "MidiInputRouting"
    ; path = "/LiveSet/Tracks/*/DeviceChain/MidiInputRouting" }
  ; { id = 32; category = Routing; name = "MidiOutputRouting"
    ; path = "/LiveSet/Tracks/*/DeviceChain/MidiOutputRouting" }
  ; { id = 33; category = Routing; name = "Target"
    ; path = "/LiveSet/Tracks/*/DeviceChain/*/Target" }
  ; { id = 34; category = Routing; name = "UpperDisplayString"
    ; path = "/LiveSet/Tracks/*/DeviceChain/*/UpperDisplayString" }
  (* F. Automation — from automation.ml Automation.create *)
  ; { id = 35; category = Automation; name = "EnvelopeTarget-PointeeId"
    ; path = "/**/AutomationEnvelope/EnvelopeTarget/PointeeId" }
  ; { id = 36; category = Automation; name = "Automation-Events-Regex"
    ; path = "/**/Automation/Events/'(Float\\|Int\\|Enum)Event'" }
  ; { id = 37; category = Automation; name = "Automation-Events-Wildcard"
    ; path = "/**/Automation/Events/*" }
  (* G. Device Common — from device.ml all device create functions *)
  ; { id = 38; category = DeviceCommon; name = "Pointee"
    ; path = "/**/Pointee" }
  ; { id = 39; category = DeviceCommon; name = "LastPresetRef-Value"
    ; path = "/**/LastPresetRef/Value/*" }
  ; { id = 40; category = DeviceCommon; name = "On"
    ; path = "/**/On" }
  ; { id = 41; category = DeviceCommon; name = "UserName"
    ; path = "/**/UserName" }
  ; { id = 42; category = DeviceCommon; name = "ShouldShowPresetName"
    ; path = "/**/ShouldShowPresetName" }
  ; { id = 43; category = DeviceCommon; name = "LomId-Manual-parent"
    ; path = "/**/LomId/../Manual/.." }
  ; { id = 44; category = DeviceCommon; name = "Tracks-wildcard"
    ; path = "/LiveSet/Tracks/*" }
  ; { id = 45; category = DeviceCommon; name = "Manual"
    ; path = "/**/Manual" }
  (* H. Plugin/M4L/Group Devices — from device.ml specialized creates *)
  ; { id = 46; category = PluginM4LGroup; name = "PluginDesc"
    ; path = "/**/PluginDesc" }
  ; { id = 47; category = PluginM4LGroup; name = "ParameterList-wildcard"
    ; path = "/**/ParameterList/*" }
  ; { id = 48; category = PluginM4LGroup; name = "ParameterValue"
    ; path = "/**/ParameterValue" }
  ; { id = 49; category = PluginM4LGroup; name = "MxPatchRef"
    ; path = "/**/MxPatchRef" }
  ; { id = 50; category = PluginM4LGroup; name = "MxDFloatParameter"
    ; path = "/**/MxDFloatParameter" }
  ; { id = 51; category = PluginM4LGroup; name = "MxDIntParameter"
    ; path = "/**/MxDIntParameter" }
  ; { id = 52; category = PluginM4LGroup; name = "MxDBoolParameter"
    ; path = "/**/MxDBoolParameter" }
  ; { id = 53; category = PluginM4LGroup; name = "MxDEnumParameter"
    ; path = "/**/MxDEnumParameter" }
  ; { id = 54; category = PluginM4LGroup; name = "Branches"
    ; path = "/**/Branches" }
  ; { id = 55; category = PluginM4LGroup; name = "MacroDisplayNames-regex"
    ; path = "/**/'MacroDisplayNames\\.[0-9]+'" }
  ; { id = 56; category = PluginM4LGroup; name = "MacroControls-regex"
    ; path = "/**/'MacroControls\\.[0-9]+'" }
  ; { id = 57; category = PluginM4LGroup; name = "MacroSnapshot"
    ; path = "/**/MacroVariations/MacroSnapshots/MacroSnapshot" }
  ; { id = 58; category = PluginM4LGroup; name = "MacroValues-regex"
    ; path = "/**/'MacroValues\\.[0-9]+'" }
  ; { id = 59; category = PluginM4LGroup; name = "MixerDevice"
    ; path = "/**/MixerDevice" }
  (* I. Clip Queries — from clip.ml MidiClip/AudioClip/Loop/Fade.create *)
  ; { id = 60; category = Clip; name = "MidiClip-Name"
    ; path = "/**/MidiClip/Name" }
  ; { id = 61; category = Clip; name = "MidiClip-CurrentStart"
    ; path = "/**/MidiClip/CurrentStart" }
  ; { id = 62; category = Clip; name = "MidiClip-CurrentEnd"
    ; path = "/**/MidiClip/CurrentEnd" }
  ; { id = 63; category = Clip; name = "MidiClip-Loop"
    ; path = "/**/MidiClip/Loop" }
  ; { id = 64; category = Clip; name = "Loop-LoopStart"
    ; path = "/**/Loop/LoopStart" }
  ; { id = 65; category = Clip; name = "Loop-LoopOn"
    ; path = "/**/Loop/LoopOn" }
  ; { id = 66; category = Clip; name = "TimeSignature"
    ; path = "/**/TimeSignature/TimeSignatures/RemoteableTimeSignature" }
  ; { id = 67; category = Clip; name = "KeyTrack"
    ; path = "/**/MidiClip/Notes/KeyTracks/KeyTrack" }
  ; { id = 68; category = Clip; name = "MidiNoteEvent"
    ; path = "/**/MidiClip/Notes/KeyTracks/KeyTrack/Notes/MidiNoteEvent" }
  ; { id = 69; category = Clip; name = "AudioClip-SampleRef"
    ; path = "/**/AudioClip/SampleRef" }
  ; { id = 70; category = Clip; name = "AudioClip-Fade"
    ; path = "/**/AudioClip/Fade" }
  ; { id = 71; category = Clip; name = "AudioClip-Fades"
    ; path = "/**/AudioClip/Fades" }
  (* J. Locator — from liveset.ml Locator.create *)
  ; { id = 72; category = Locator; name = "Locator-Name"
    ; path = "/LiveSet/Locators/Locators/Locator/Name" }
  ; { id = 73; category = Locator; name = "Locator-Time"
    ; path = "/LiveSet/Locators/Locators/Locator/Time" }
  ]

(* --- DOM pipeline --- *)

let run_dom filename =
  let xml_str = File.decompress_als_to_string filename in
  let dom = Xml.read_string xml_str in
  List.map (fun q ->
      let matches = Upath.find_all q.path dom in
      (q.id, List.length matches)
    ) queries

(* --- Streaming pipeline --- *)

let to_upath2_query q : Upath2.query =
  { Upath2.qid = q.id; path = Upath.parse_path q.path; attr = None }

let run_streaming filename =
  let xml_str = File.decompress_als_to_string filename in
  let nfa = Upath2.compile (List.map to_upath2_query queries) in
  let stream = Xml2.stream_from_string xml_str in
  let results = Upath2.evaluate nfa stream in
  let counts = Hashtbl.create 128 in
  List.iter (fun r ->
      let cur =
        try Hashtbl.find counts r.Upath2.query_id with Not_found -> 0
      in
      Hashtbl.replace counts r.Upath2.query_id (cur + 1)
    ) results;
  List.map (fun q ->
      let n = try Hashtbl.find counts q.id with Not_found -> 0 in
      (q.id, n)
    ) queries

(* --- Per-category reporting --- *)

let report_categories results =
  let by_cat = Hashtbl.create 16 in
  List.iter (fun q ->
      let lst = try Hashtbl.find by_cat q.category with Not_found -> [] in
      Hashtbl.replace by_cat q.category (q :: lst)
    ) queries;
  let cat_counts = Hashtbl.create 128 in
  List.iter (fun (qid, n) -> Hashtbl.replace cat_counts qid n) results;
  let cats =
    [ LivesetRoot; TrackNames; TrackStructure; MixerParams; Routing
    ; Automation; DeviceCommon; PluginM4LGroup; Clip; Locator ]
  in
  Printf.eprintf "\n%-18s %4s %6s\n%!" "Category" "Qs" "Matches";
  Printf.eprintf "%s\n%!" (String.make 30 '-');
  List.iter (fun cat ->
      let qs = try Hashtbl.find by_cat cat with Not_found -> [] in
      let total =
        List.fold_left (fun acc q ->
            acc + (try Hashtbl.find cat_counts q.id with Not_found -> 0)
          ) 0 qs
      in
      Printf.eprintf "%-18s %4d %6d\n%!"
        (category_to_string cat) (List.length qs) total;
      List.iter (fun q ->
          let n =
            try Hashtbl.find cat_counts q.id with Not_found -> 0
          in
          Printf.eprintf "  %2d %-30s %4d\n%!" q.id q.name n
        ) (List.rev qs)
    ) cats

(* --- Main --- *)

let bench filename mode : int =
  match mode with
  | Dom ->
    let t0 = Sys.time () in
    let results = run_dom filename in
    let t1 = Sys.time () in
    let total_matches =
      List.fold_left (fun acc (_, n) -> acc + n) 0 results
    in
    Printf.printf "dom\t%f\t%d\t%d\n"
      (t1 -. t0) (List.length queries) total_matches;
    report_categories results;
    0
  | Streaming ->
    let t0 = Sys.time () in
    let results = run_streaming filename in
    let t1 = Sys.time () in
    let total_matches =
      List.fold_left (fun acc (_, n) -> acc + n) 0 results
    in
    Printf.printf "streaming\t%f\t%d\t%d\n"
      (t1 -. t0) (List.length queries) total_matches;
    report_categories results;
    0

(* --- CLI --- *)

let file_arg =
  let doc = "Path to .als file (gzip-compressed XML)." in
  Cmdliner.Arg.(required & pos 0 (some string) None & info [] ~docv:"FILE" ~doc)

let mode_arg =
  let doc = "Pipeline to benchmark: dom or streaming." in
  Cmdliner.Arg.(
    required
    & opt (some (enum [ "dom", Dom; "streaming", Streaming ])) None
    & info [ "mode" ] ~docv:"MODE" ~doc)

let cmd =
  let+ file = file_arg
  and+ mode = mode_arg in
  bench file mode

let () =
  let info =
    Cmdliner.Cmd.info "bench-xml"
      ~doc:"Benchmark DOM vs Streaming XML query pipelines."
  in
  exit (Cmdliner.Cmd.eval' (Cmdliner.Cmd.v info cmd))
