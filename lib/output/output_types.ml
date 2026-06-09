type note_display_style = Sharp | Flat [@@deriving yojson, jsonschema]

let note_display_style_to_json x = Yojson.Safe.to_basic (note_display_style_to_yojson x)

type time_format = QuarterNotes | BeatTime | RealTime [@@deriving yojson, jsonschema]

let time_format_to_json x = Yojson.Safe.to_basic (time_format_to_yojson x)

let time_format_equal a b = match a, b with
  | QuarterNotes, QuarterNotes -> true
  | BeatTime, BeatTime -> true
  | RealTime, RealTime -> true
  | _ -> false

type field_value =
  | Fint of int
  | Ffloat of float
  | Fbool of bool
  | Fstring of string

let int_value x = Fint x
let float_value x = Ffloat x
let bool_value x = Fbool x
let string_value x = Fstring x

type change_type =
  | Unchanged
  | Added
  | Removed
  | Modified
[@@deriving yojson]

type domain_type =
  | DTLiveset
  | DTTrack
  | DTDevice
  | DTClip
  | DTArrangementClip
  | DTTakeClip
  | DTAutomation
  | DTMixer
  | DTRouting
  | DTLocator
  | DTParam
  | DTNote
  | DTEvent
  | DTSend
  | DTPreset
  | DTMacro
  | DTSnapshot
  | DTLoop
  | DTSignature
  | DTSampleRef
  | DTVersion
  | DTOther
[@@deriving yojson, jsonschema]
