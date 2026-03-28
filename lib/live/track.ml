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
    audio_in : Routing.t;
    audio_out : Routing.t;
    midi_in : Routing.t;
    midi_out : Routing.t;
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
