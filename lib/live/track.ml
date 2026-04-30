open Alsdiff_base
open Alsdiff_base.Diff


module Routing = struct
  type route_type =
    | MidiIn
    | MidiOut
    | AudioIn
    | AudioOut
  [@@deriving eq]

  type t = {
    route_type : route_type;  [@id.id]
    target : string;
    upper_string : string;
    lower_string : string;
  } [@@deriving eq, id, patch] [@@patch.generate_diff]

  (** Parse route type from XML element name *)
  let parse_route_type xml =
    match Xml.get_name xml with
    | "MidiInputRouting" -> MidiIn
    | "MidiOutputRouting" -> MidiOut
    | "AudioInputRouting" -> AudioIn
    | "AudioOutputRouting" -> AudioOut
    | name -> raise (Xml.Xml_error (xml, "Invalid routing element: " ^ name))

  (** [create xml] creates a routing object from an XML element *)
  let create (xml : Xml.t) : t =
    match xml with
    | Xml.Element _ ->
      let route_type = parse_route_type xml in
      let target = Upath.get_attr "/Target" "Value" xml in
      let upper_string = Upath.get_attr "/UpperDisplayString" "Value" xml in
      let lower_string = Upath.get_attr "/LowerDisplayString" "Value" xml in

      { route_type; target; upper_string; lower_string }
    | Xml.Data _ ->
      raise (Xml.Xml_error (xml, "Invalid XML element for creating Routing"))

end


module RoutingSet = struct
  type t = {
    audio_in : Routing.t;   [@id.ref]
    audio_out : Routing.t;  [@id.ref]
    midi_in : Routing.t;    [@id.ref]
    midi_out : Routing.t;   [@id.ref]
  } [@@deriving eq, patch] [@@patch.generate_diff]

  let create (xml : Xml.t) : t =
    let audio_in = Upath.find "AudioInputRouting" xml |> snd |> Routing.create in
    let audio_out = Upath.find "AudioOutputRouting" xml |> snd |> Routing.create in
    let midi_in = Upath.find "MidiInputRouting" xml |> snd |> Routing.create in
    let midi_out = Upath.find "MidiOutputRouting" xml |> snd |> Routing.create in
    { audio_in; audio_out; midi_in; midi_out }

end


(* ================== Mixer module ================== *)
module GenericParam = Device.GenericParam
module NameIdGenericParam = Device.NameIdGenericParam

module Send = struct
  type t = {
    id : int;                   [@id.id] [@patch.skip]
    amount : GenericParam.t;
  } [@@deriving eq, id, patch] [@@patch.generate_diff]

  (** Create [Send.t] from XML element.
      @param xml XML element [<TrackHolder Id="N">...</TrackHolder>] *)
  let create (xml : Xml.t) : t =
    let id = Xml.get_int_attr "Id" xml in
    let amount = Upath.find "/Send" xml |> snd |> GenericParam.create_float_manual in
    { id; amount }

end


module Mixer = struct
  type t = {
    volume : GenericParam.t;
    pan : GenericParam.t;
    mute : GenericParam.t;
    solo : GenericParam.t;
    sends : Send.t list;
  } [@@deriving eq, patch] [@@patch.generate_diff]

  let create (xml : Xml.t) : t =
    let volume = Upath.find "/Volume" xml |> snd |> GenericParam.create_float_manual in
    let pan = Upath.find "/Pan" xml |> snd |> GenericParam.create_float_manual in
    let mute = Upath.find "/On" xml |> snd |> GenericParam.create_bool_manual in

    (* SoloSink has a different structure - it's just <SoloSink Value="..."/> without Manual element *)
    (* We need to wrap it to make it compatible with Device.GenericParam.create *)
    let solo_value = Upath.get_bool_attr "/SoloSink" "Value" xml in
    let mapping =
      Upath.find_opt "/HeadKeyMidi" xml
      |> Option.map snd
      |> Option.map Device.MIDIMapping.create_head_key_midi
    in
    let solo = {
      GenericParam.name = "SoloSink";
      value = Bool solo_value;
      automation = 0;
      modulation = 0;
      mapping;
    } in
    let sends = xml
      |> Upath.find_all "/Sends/TrackSendHolder"
      |> List.map (fun (_, xml) -> Send.create xml)
    in
    { volume; pan; mute; solo; sends }

end


module MidiTrack = struct
  type t = {
    id : int;                     [@id.id] [@patch.identity]
    name : string;
    current_name : string;        [@patch.identity]
    clips : Clip.MidiClip.t list;
    automations : Automation.t list;
    devices : Device.t list;
    mixer : Mixer.t;
    routings : RoutingSet.t;
  } [@@deriving eq, id, patch] [@@patch.generate_diff]

  let create (xml : Xml.t) : t =
    let id = Xml.get_int_attr "Id" xml in
    let name = Upath.get_attr "/Name/EffectiveName" "Value" xml in
    let automations =
      Upath.find_all_seq "/AutomationEnvelopes/*/AutomationEnvelope" xml
      |> Seq.map (fun x -> x |> snd |> Automation.create)
      |> List.of_seq in
    let clips = Upath.find_all_seq "/**/ClipTimeable/ArrangerAutomation/Events/MidiClip" xml
      |> Seq.map (fun x -> x |> snd |> Clip.MidiClip.create)
      |> List.of_seq in
    let devices = Upath.find_all_seq "/DeviceChain/*/Devices" xml
      |> Seq.map snd
      |> Seq.concat_map (fun devs ->
          Xml.get_childs devs |> List.to_seq |> Seq.map Device.create)
      |> List.of_seq in
    let mixer = Upath.find "/DeviceChain/Mixer" xml |> snd |> Mixer.create in
    let routings = Upath.find "/DeviceChain" xml |> snd |> RoutingSet.create in

    { id; name; current_name = name; clips; automations; devices; mixer; routings }

end


module AudioTrack = struct
  type t = {
    id : int;                     [@id.id] [@patch.identity]
    name : string;
    current_name : string;        [@patch.identity]
    clips : Clip.AudioClip.t list;
    automations : Automation.t list;
    devices : Device.t list;
    mixer : Mixer.t;
    routings : RoutingSet.t;
  } [@@deriving eq, id, patch] [@@patch.generate_diff]

  let create (xml : Xml.t) : t =
    let id = Xml.get_int_attr "Id" xml in
    let name = Upath.get_attr "/Name/EffectiveName" "Value" xml in
    let automations =
      Upath.find_all_seq "/AutomationEnvelopes/*/AutomationEnvelope" xml
      |> Seq.map (fun x -> x |> snd |> Automation.create)
      |> List.of_seq in
    let clips = Upath.find_all_seq "/**/AudioClip" xml
      |> Seq.map (fun x -> x |> snd |> Clip.AudioClip.create)
      |> List.of_seq in
    let devices = Upath.find_all_seq "/DeviceChain/*/Devices" xml
      |> Seq.map snd
      |> Seq.concat_map (fun devs ->
          Xml.get_childs devs |> List.to_seq |> Seq.map Device.create)
      |> List.of_seq in
    let mixer = Upath.find "/DeviceChain/Mixer" xml |> snd |> Mixer.create in
    let routings = Upath.find "/DeviceChain" xml |> snd |> RoutingSet.create in
    { id; name; current_name = name; clips; automations; devices; mixer; routings }

end


module MainMixer = struct
  type t = {
    base : Mixer.t;
    tempo : GenericParam.t;
    time_signature : GenericParam.t; (* TODO: how to parse the time signature number? *)
    crossfade : GenericParam.t;
    global_groove : GenericParam.t;
  } [@@deriving eq, patch] [@@patch.generate_diff]

  let create (xml : Xml.t) : t =
    let base = Mixer.create xml in
    let tempo = Upath.find "/Tempo" xml |> snd |> GenericParam.create_float_manual in
    let time_signature = Upath.find "/TimeSignature" xml |> snd |> GenericParam.create_int_manual in
    let crossfade = Upath.find "/CrossFade" xml |> snd |> GenericParam.create_float_manual in
    let global_groove = Upath.find "/GlobalGrooveAmount" xml |> snd |> GenericParam.create_float_manual in
    { base; tempo; time_signature; crossfade; global_groove; }

end


module MainTrack = struct
  type t = {
    name : string;
    current_name : string;        [@patch.identity]
    automations : Automation.t list;
    devices : Device.t list;
    mixer : MainMixer.t;
    routings : RoutingSet.t;
  } [@@deriving eq, patch] [@@patch.generate_diff]

  let create (xml : Xml.t) : t =
    let name = Upath.get_attr "/Name/EffectiveName" "Value" xml in
    let automations =
      Upath.find_all_seq "/AutomationEnvelopes/*/AutomationEnvelope" xml
      |> Seq.map (fun x -> x |> snd |> Automation.create)
      |> List.of_seq in
    let devices = Upath.find_all_seq "/DeviceChain/*/Devices" xml
      |> Seq.map snd
      |> Seq.concat_map (fun devs ->
          Xml.get_childs devs |> List.to_seq |> Seq.map Device.create)
      |> List.of_seq in
    let mixer = Upath.find "/DeviceChain/Mixer" xml |> snd |> MainMixer.create in
    let routings = Upath.find "/DeviceChain" xml |> snd |> RoutingSet.create in
    { name; current_name = name; automations; devices; mixer; routings }

  let decode_time_signature (code : int) : Clip.TimeSignature.t =
    let denom_index = code / 99 in
    let numer = (code mod 99) + 1 in
    let denom = 1 lsl denom_index in
    { Clip.TimeSignature.numer; denom }

  let get_time_signature_events (t : t) : (float * Clip.TimeSignature.t) list =
    let target_id = t.mixer.time_signature.GenericParam.automation in
    let automation =
      List.find_opt (fun (a : Automation.t) -> a.target = target_id) t.automations
    in
    match automation with
    | None -> []
    | Some auto ->
      List.filter_map (fun (e : Automation.EnvelopeEvent.t) ->
          match e.value with
          | EnumEvent code -> Some (e.time, decode_time_signature code)
          | _ -> None
        ) auto.events

  let time_to_position (events : (float * Clip.TimeSignature.t) list) (time : float) :
    int * int * int =
    if time <= 0.0 then (1, 1, 1)
    else begin
      let events = List.sort (fun (t1, _) (t2, _) -> Float.compare t1 t2) events in
      let initial_ts = match events with
        | [] -> { Clip.TimeSignature.numer = 4; denom = 4 }
        | (_, ts) :: _ -> ts
      in
      let real_events = List.filter (fun (t, _) -> t >= 0.0) events in
      let qn_per_bar (ts : Clip.TimeSignature.t) =
        float_of_int ts.numer *. 4.0 /. float_of_int ts.denom
      in
      let qn_per_beat (ts : Clip.TimeSignature.t) =
        4.0 /. float_of_int ts.denom
      in
      let position_in_segment ts cum_bars seg_start =
        let remaining = time -. seg_start in
        let bar_off = int_of_float (remaining /. qn_per_bar ts) in
        let rem_bar = remaining -. float_of_int bar_off *. qn_per_bar ts in
        let beat_off = int_of_float (rem_bar /. qn_per_beat ts) in
        let rem_beat = rem_bar -. float_of_int beat_off *. qn_per_beat ts in
        let sixteenth_off = int_of_float (rem_beat *. 4.0) in
        (cum_bars + bar_off + 1, beat_off + 1, sixteenth_off + 1)
      in
      let rec walk evts cum_bars seg_start ts = match evts with
        | [] -> position_in_segment ts cum_bars seg_start
        | (evt_time, evt_ts) :: rest when evt_time <= time ->
          let bars = int_of_float (Float.round ((evt_time -. seg_start) /. qn_per_bar ts)) in
          walk rest (cum_bars + bars) evt_time evt_ts
        | _ -> position_in_segment ts cum_bars seg_start
      in
      walk real_events 0 0.0 initial_ts
    end

  let get_tempo_events (t : t) : (float * float * Automation.CurveControls.t option) list =
    let target_id = t.mixer.tempo.GenericParam.automation in
    let automation =
      List.find_opt (fun (a : Automation.t) -> a.target = target_id) t.automations
    in
    match automation with
    | None -> []
    | Some auto ->
      List.filter_map (fun (e : Automation.EnvelopeEvent.t) ->
          match e.value with
          | FloatEvent bpm -> Some (e.time, bpm, e.curve)
          | _ -> None
        ) auto.events

  let time_to_realtime (quarter_notes : float)
      (tempo_events : (float * float * Automation.CurveControls.t option) list) :
    int * int * int =
    if quarter_notes <= 0.0 then (0, 0, 0)
    else begin
      let events =
        List.sort (fun (t1, _, _) (t2, _, _) -> Float.compare t1 t2) tempo_events
      in
      let initial_tempo = match events with
        | [] -> 120.0
        | (_, bpm, _) :: _ -> bpm
      in
      let real_events = List.filter (fun (t, _, _) -> t >= 0.0) events in
      let bezier_coord s c1 c2 =
        let s1 = 1.0 -. s in
        3.0 *. s1 *. s1 *. s *. c1 +. 3.0 *. s1 *. s *. s *. c2 +. s *. s *. s
      in
      let solve_bezier_x target c1x c2x =
        let lo = ref 0.0 in
        let hi = ref 1.0 in
        for _ = 1 to 30 do
          let mid = (!lo +. !hi) /. 2.0 in
          if bezier_coord mid c1x c2x < target
          then lo := mid
          else hi := mid
        done;
        (!lo +. !hi) /. 2.0
      in
      let linear_seconds dq t_start t_end =
        if dq <= 0.0 then 0.0
        else if Float.abs (t_end -. t_start) < 0.0001 then
          dq *. 60.0 /. t_start
        else
          60.0 *. dq /. (t_end -. t_start) *. Float.log (t_end /. t_start)
      in
      let bezier_seconds dq t_prev t_next curve =
        let open Automation.CurveControls in
        let n = 500 in
        let tempo_at_n i =
          let frac = float_of_int i /. float_of_int n in
          let s = solve_bezier_x frac curve.curve1_x curve.curve2_x in
          let y = bezier_coord s curve.curve1_y curve.curve2_y in
          t_prev +. (t_next -. t_prev) *. y
        in
        let h = dq /. float_of_int n in
        let f i = 60.0 /. tempo_at_n i in
        let sum =
          f 0 +. f n
          +. 4.0 *. (let s = ref 0.0 in
                     for i = 1 to n - 1 do
                       if i mod 2 = 1 then s := !s +. f i
                     done;
                     !s)
          +. 2.0 *. (let s = ref 0.0 in
                     for i = 1 to n - 1 do
                       if i mod 2 = 0 then s := !s +. f i
                     done;
                     !s)
        in
        h /. 3.0 *. sum
      in
      let rec walk evts cum_sec seg_start tempo prev_curve = match evts with
        | [] ->
          cum_sec +. (quarter_notes -. seg_start) *. 60.0 /. tempo
        | (evt_time, evt_tempo, evt_curve) :: rest when evt_time <= quarter_notes ->
          let dq = evt_time -. seg_start in
          let sec = match prev_curve with
            | None -> linear_seconds dq tempo evt_tempo
            | Some curve -> bezier_seconds dq tempo evt_tempo curve
          in
          walk rest (cum_sec +. sec) evt_time evt_tempo evt_curve
        | (evt_time, evt_tempo, _) :: _ ->
          let remaining = quarter_notes -. seg_start in
          let sec = match prev_curve with
            | None ->
              let total = evt_time -. seg_start in
              let t_at_target = tempo +. (evt_tempo -. tempo) *. remaining /. total in
              linear_seconds remaining tempo t_at_target
            | Some curve ->
              let open Automation.CurveControls in
              let n = 500 in
              let norm_end = remaining /. (evt_time -. seg_start) in
              let tempo_at_n i =
                let frac = float_of_int i /. float_of_int n *. norm_end in
                let s = solve_bezier_x frac curve.curve1_x curve.curve2_x in
                let y = bezier_coord s curve.curve1_y curve.curve2_y in
                tempo +. (evt_tempo -. tempo) *. y
              in
              let h = remaining /. float_of_int n in
              let f i = 60.0 /. tempo_at_n i in
              let sum =
                f 0 +. f n
                +. 4.0 *. (let s = ref 0.0 in
                           for i = 1 to n - 1 do
                             if i mod 2 = 1 then s := !s +. f i
                           done;
                           !s)
                +. 2.0 *. (let s = ref 0.0 in
                           for i = 1 to n - 1 do
                             if i mod 2 = 0 then s := !s +. f i
                           done;
                           !s)
              in
              h /. 3.0 *. sum
          in
          cum_sec +. sec
      in
      let initial_curve = match events with
        | [] -> None
        | (_, _, c) :: _ -> c
      in
      let total_ms =
        int_of_float (Float.round (walk real_events 0.0 0.0 initial_tempo initial_curve *. 1000.0))
      in
      let minutes = total_ms / 60_000 in
      let rem = total_ms mod 60_000 in
      (minutes, rem / 1000, rem mod 1000)
    end

  (* MainTrack is also a singleton *)
  let has_same_id _ _ = true
  let id_hash _ = Hashtbl.hash 0

end

(* Sum type that represents either a MidiTrack or AudioTrack *)
type t =
  | Midi of MidiTrack.t
  | Audio of AudioTrack.t
  | Group of AudioTrack.t
  | Return of AudioTrack.t
  | Main of MainTrack.t
[@@deriving eq]

let has_same_id old_track new_track =
  match old_track, new_track with
  | Midi old_midi, Midi new_midi -> MidiTrack.has_same_id old_midi new_midi
  | Audio old_audio, Audio new_audio
  | Group old_audio, Group new_audio
  | Return old_audio, Return new_audio -> AudioTrack.has_same_id old_audio new_audio
  | Main old_main, Main new_main -> MainTrack.has_same_id old_main new_main
  | _ -> false

let id_hash = function
  | Midi midi -> MidiTrack.id_hash midi
  | Group audio | Audio audio | Return audio -> AudioTrack.id_hash audio
  | Main main -> MainTrack.id_hash main

let create (xml : Xml.t) : t =
  match xml with
  | Xml.Element { name = "MidiTrack"; _ } -> Midi (MidiTrack.create xml)
  | Xml.Element { name = "AudioTrack"; _ } -> Audio (AudioTrack.create xml)
  | Xml.Element { name = "GroupTrack"; _ } -> Group (AudioTrack.create xml)
  | Xml.Element { name = "ReturnTrack"; _ } -> Return (AudioTrack.create xml)
  | Xml.Element { name = "MainTrack"; _ } -> Main (MainTrack.create xml)
  | _ ->
    let name = match xml with
      | Xml.Element { name; _ } -> name
      | _ -> "non-element"
    in
    raise (Xml.Xml_error (xml, "Unsupported track type: " ^ name))

module Patch = struct
  type t =
    | MidiPatch of MidiTrack.Patch.t
    | AudioPatch of AudioTrack.Patch.t
    | MainPatch of MainTrack.Patch.t

  let is_empty = function
    | MidiPatch patch -> MidiTrack.Patch.is_empty patch
    | AudioPatch patch -> AudioTrack.Patch.is_empty patch
    | MainPatch patch -> MainTrack.Patch.is_empty patch
end

let get_name = function
  | Midi a -> a.name
  | Audio a -> a.name
  | Group a -> a.name
  | Return a -> a.name
  | Main _ -> "Main"

let type_name = function
  | Midi _ -> "MidiTrack"
  | Audio _ -> "AudioTrack"
  | Group _ -> "GroupTrack"
  | Return _ -> "ReturnTrack"
  | Main _ -> "MainTrack"

let diff (old_track : t) (new_track : t) : Patch.t =
  match old_track, new_track with
  | Midi old_midi, Midi new_midi ->
    let midi_patch = MidiTrack.diff old_midi new_midi in
    Patch.MidiPatch midi_patch
  | Audio old_audio, Audio new_audio
  | Group old_audio, Group new_audio
  | Return old_audio, Return new_audio ->
    let audio_patch = AudioTrack.diff old_audio new_audio in
    Patch.AudioPatch audio_patch
  | Main old_main, Main new_main ->
    let main_patch = MainTrack.diff old_main new_main in
    Patch.MainPatch main_patch
  | _ ->
    failwith (Printf.sprintf "Cannot diff tracks of different types: %s (%s) vs %s (%s)"
                (type_name old_track) (get_name old_track)
                (type_name new_track) (get_name new_track))
