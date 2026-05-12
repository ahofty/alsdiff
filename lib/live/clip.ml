open Alsdiff_base
open Alsdiff_base.Diff


module TimeSignature = struct
  type t = {
    numer : int;
    denom : int;
  } [@@deriving eq, patch, view_spec] [@@patch.generate_diff]
  let create (xml : Xml.t) : t =
    match xml with
    | Xml.Element { name = "RemoteableTimeSignature"; _ } ->
      let numer = Upath.get_int_attr "/Numerator" "Value" xml in
      let denom = Upath.get_int_attr "/Denominator" "Value" xml in
      { numer; denom }
    | _ -> raise (Xml.Xml_error (xml, "Invalid XML element for creating TimeSignature"))
end


module MidiNote = struct
  type t = {
    id : int; [@id.id] [@patch.skip] [@view.skip]
    note : int;
    time : float; [@view.scalar time]
    duration : float;
    velocity : float;
    off_velocity : float;
  } [@@deriving eq, id, patch, view_spec] [@@patch.generate_diff]

  let create (note: int) (xml : Xml.t) : t =
    match xml with
    | Xml.Element { name = "MidiNoteEvent"; _ } ->
      let id = Xml.get_int_attr "NoteId" xml in
      let time = Xml.get_float_attr "Time" xml in
      let duration = Xml.get_float_attr "Duration" xml in
      let velocity = Xml.get_float_attr "Velocity" xml in
      let off_velocity = Xml.get_float_attr "OffVelocity" xml in
      { id; note; time; duration; velocity; off_velocity }
    | _ -> raise (Xml.Xml_error (xml, "Invalid XML element for creating MidiNote"))
end

module Loop = struct
  type t = {
    start_time : float; [@view.scalar time]
    end_time : float; [@view.scalar time]
    on : bool;
  } [@@deriving eq, patch, view_spec] [@@patch.generate_diff]


  let create (xml : Xml.t) : t =
    match xml with
    | Xml.Element { name = "Loop"; _ } ->
      let start = Upath.get_float_attr "/LoopStart" "Value" xml in
      let end_ = Upath.get_float_attr "/LoopEnd" "Value" xml in
      let on = Upath.get_bool_attr "/LoopOn" "Value" xml in
      { start_time = start; end_time = end_; on; }
    | _ -> raise (Xml.Xml_error (xml, "Invalid XML element for creating Loop"))
end


module MidiClip = struct
  type t = {
    id : int; [@id.id] [@patch.identity] [@view.skip]
    name : string;
    start_time : float; [@view.scalar time]
    end_time : float; [@view.scalar time]
    loop : Loop.t; [@view.skip]
    signature : TimeSignature.t; [@view.child "DTSignature"] [@view.label "TimeSignature"]
    notes : MidiNote.t list; [@view.collection "DTNote"] [@view.builder "build_notes"]
  } [@@deriving eq, id, patch, view_spec] [@@patch.generate_diff]

  let create (xml : Xml.t) : t =
    match xml with
    | Xml.Element { name = "MidiClip"; _ } ->
      let id = Xml.get_int_attr "Id" xml in
      let name = Upath.get_attr "/Name" "Value" xml in
      let start_time = Upath.get_float_attr "/CurrentStart" "Value" xml in

      (* Extract end time from CurrentEnd *)
      let end_time = Upath.get_float_attr "/CurrentEnd" "Value" xml in

      (* Extract loop information *)
      let loop = Upath.find "/Loop" xml |> snd |> Loop.create in

      (* Extract time signature *)
      let signature = Upath.find "/TimeSignature/TimeSignatures/RemoteableTimeSignature" xml |> snd |> TimeSignature.create in

      (* Extract MIDI notes from KeyTracks *)
      let notes = Upath.find_all_seq "/Notes/KeyTracks/KeyTrack" xml
        |> Seq.map snd
        |> Seq.flat_map (fun keytrack ->
            let key = Upath.get_int_attr "MidiKey" "Value" keytrack in
            Upath.find_all_seq "/Notes/MidiNoteEvent" keytrack
            |> Seq.map snd
            |> Seq.map @@ MidiNote.create key)
        |> List.of_seq
      in

      { id; name; start_time; end_time; loop; signature; notes }
    | _ -> raise (Xml.Xml_error (xml, "Expected MidiClip element"))
end


module SampleRef = struct
  type t = {
    file_path : string;
    crc : string;
    last_modified_date : int; [@view.scalar unix_timestamp]
  } [@@deriving eq, patch, view_spec] [@@patch.generate_diff]

  let create (xml : Xml.t) : t =
    match xml with
    | Xml.Element { name = "SampleRef"; _ } ->
      let last_modified_date = Upath.get_int_attr "LastModDate" "Value" xml in
      let file_path = Upath.get_attr "FileRef/Path" "Value" xml in
      let crc = Upath.get_attr "FileRef/OriginalCrc" "Value" xml in
      { file_path; crc; last_modified_date }
    | _ -> raise (Xml.Xml_error (xml, "Invalid XML element for creating SampleRef"))

end


module Fade = struct
  type t = {
    fade_in_length : float;
    fade_out_length : float;
    is_initialized : bool;
    crossfade_state : int;
    fade_in_curve_skew : float;
    fade_in_curve_slope : float;
    fade_out_curve_skew : float;
    fade_out_curve_slope : float;
    is_default_fade_in : bool;
    is_default_fade_out : bool;
  } [@@deriving eq, patch, view_spec] [@@patch.generate_diff]

  let create (xml : Xml.t) : t =
    match xml with
    | Xml.Element { name = "Fades"; _ } ->
      let fade_in_length = Upath.get_float_attr "/FadeInLength" "Value" xml in
      let fade_out_length = Upath.get_float_attr "/FadeOutLength" "Value" xml in
      let is_initialized = Upath.get_bool_attr "/ClipFadesAreInitialized" "Value" xml in
      let crossfade_state = Upath.get_int_attr "/CrossfadeInState" "Value" xml in
      let fade_in_curve_skew = Upath.get_float_attr "/FadeInCurveSkew" "Value" xml in
      let fade_in_curve_slope = Upath.get_float_attr "/FadeInCurveSlope" "Value" xml in
      let fade_out_curve_skew = Upath.get_float_attr "/FadeOutCurveSkew" "Value" xml in
      let fade_out_curve_slope = Upath.get_float_attr "/FadeOutCurveSlope" "Value" xml in
      let is_default_fade_in = Upath.get_bool_attr "/IsDefaultFadeIn" "Value" xml in
      let is_default_fade_out = Upath.get_bool_attr "/IsDefaultFadeOut" "Value" xml in
      {
        fade_in_length; fade_out_length; is_initialized;
        crossfade_state; fade_in_curve_skew; fade_in_curve_slope;
        fade_out_curve_skew; fade_out_curve_slope;
        is_default_fade_in; is_default_fade_out;
      }
    | _ -> raise (Xml.Xml_error (xml, "Invalid XML element for creating Fade"))

end


module AudioClip = struct
  (* TODO: support warp related settings *)
  type t = {
    id : int; [@id.id] [@patch.identity] [@view.skip]
    name : string;
    start_time : float; [@view.scalar time]
    end_time : float; [@view.scalar time]
    loop : Loop.t; [@view.skip]
    signature : TimeSignature.t; [@view.child "DTSignature"] [@view.label "TimeSignature"]
    sample_ref : SampleRef.t; [@view.child "DTSampleRef"] [@view.label "SampleRef"]
    fade : Fade.t option; [@view.optional_child "DTClip"]
  } [@@deriving eq, id, patch, view_spec]

  let create (xml : Xml.t) : t =
    match xml with
    | Xml.Element { name = "AudioClip"; _ } ->
      let id = Xml.get_int_attr "Id" xml in
      let name = Upath.get_attr "/Name" "Value" xml in
      let start_time = Upath.get_float_attr "/CurrentStart" "Value" xml in
      let end_time = Upath.get_float_attr "/CurrentEnd" "Value" xml in

      (* Extract loop information *)
      (* TODO: what the fuck does `StartRelative` means in the `Loop` element *)
      let loop = Upath.find "/Loop" xml |> snd |> Loop.create in
      (* Extract time signature *)
      (* TODO: support time signature automation *)
      let signature = Upath.find "/TimeSignature/TimeSignatures/RemoteableTimeSignature" xml |> snd |> TimeSignature.create in
      (* Extract sample reference *)
      let sample_ref = Upath.find "/SampleRef" xml |> snd |> SampleRef.create in

      (* Extract fade information - only parse Fades if Fade is enabled *)
      let fade =
        let fade_enabled = Upath.get_bool_attr "/Fade" "Value" xml in
        if fade_enabled then
          Some (Upath.find "/Fades" xml |> snd |> Fade.create)
        else
          None
      in

      { id; name; start_time; end_time; loop; signature; sample_ref; fade }
    | _ -> raise (Xml.Xml_error (xml, "Expected AudioClip element"))

  let diff (old_clip : t) (new_clip : t) : Patch.t =
    let { id = old_id; name = old_name; start_time = old_start; end_time = old_end; loop = old_loop; signature = old_sig; sample_ref = old_sample; fade = old_fade } = old_clip in
    let { id = new_id; name = new_name; start_time = new_start; end_time = new_end; loop = new_loop; signature = new_sig; sample_ref = new_sample; fade = new_fade } = new_clip in

    (* Only compare clips with the same id *)
    if old_id <> new_id then
      failwith "cannot diff two clips with different Id"
    else
      let name_change = diff_atomic_value (module String) old_name new_name in
      let start_time_change = diff_atomic_value (module Float) old_start new_start in
      let end_time_change = diff_atomic_value (module Float) old_end new_end in
      let loop_change = diff_complex_value (module Loop) old_loop new_loop in
      let signature_change = diff_complex_value (module TimeSignature) old_sig new_sig in
      let sample_ref_change = diff_complex_value (module SampleRef) old_sample new_sample in

      (* Handle fade diffing - both are option types *)
      let fade_change = match old_fade, new_fade with
        | None, None -> `Unchanged
        | None, Some new_fade -> `Added new_fade
        | Some old_fade, None -> `Removed old_fade
        | Some old_fade, Some new_fade ->
          let patch = Fade.diff old_fade new_fade in
          if Fade.Patch.is_empty patch then `Unchanged else `Modified patch
      in

      {
        id = new_id;
        name = name_change;
        start_time = start_time_change;
        end_time = end_time_change;
        loop = loop_change;
        signature = signature_change;
        sample_ref = sample_ref_change;
        fade = fade_change;
      }
end
