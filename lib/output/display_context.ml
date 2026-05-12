open Ptime.Span

let format_position (bar, beat, sixteenth) = Printf.sprintf "%d:%d:%d" bar beat sixteenth
let format_realtime (min, sec, ms) = Printf.sprintf "%d:%02d.%03d" min sec ms

let get_note_name_from_int ?(style : Output_types.note_display_style = Output_types.Sharp) (note_int : int) : string =
  let note_names_sharp = [| "C"; "C#"; "D"; "D#"; "E"; "F"; "F#"; "G"; "G#"; "A"; "A#"; "B" |] in
  let note_names_flat = [| "C"; "Db"; "D"; "Eb"; "E"; "F"; "Gb"; "G"; "Ab"; "A"; "Bb"; "B" |] in

  let note_class = (note_int mod 12 + 12) mod 12 in
  let octave_num = note_int / 12 - 1 in

  let note_name = match style with
    | Output_types.Sharp -> note_names_sharp.(note_class)
    | Output_types.Flat -> note_names_flat.(note_class)
  in

  Printf.sprintf "%s%d" note_name octave_num

let get_note_name ?(style : Output_types.note_display_style = Output_types.Sharp) (note : int) : string =
  get_note_name_from_int note ~style

let format_unix_timestamp (ts : int) : string =
  let span = of_int_s ts in
  match Ptime.of_span span with
  | None -> "Invalid timestamp"
  | Some t ->
    Ptime.to_rfc3339 ~space:true ~frac_s:0 ~tz_offset_s:0 t

let default_format_time (time : float) : Output_types.field_value = Output_types.Ffloat time

let format_time_str (format_time : float -> Output_types.field_value) time =
  match format_time time with
  | Output_types.Fstring s -> s
  | Output_types.Ffloat f -> Printf.sprintf "%.2f" f
  | _ -> Printf.sprintf "%.2f" time

type dual_time_formatter = {
  format_old : float -> Output_types.field_value;
  format_new : float -> Output_types.field_value;
}

let default_dual_time_formatter : dual_time_formatter = {
  format_old = default_format_time;
  format_new = default_format_time;
}
