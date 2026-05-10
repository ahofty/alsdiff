open Alsdiff_live

type scalar =
  | Raw of Output_types.field_value
  | Time of float
  | Midi_note of int
  | Unix_timestamp of int
  | Param_value of Device.param_value
  | Event_value of Automation.event_value

type nesting = Inline | Named | Optional | Collection

type label =
  | Static of string
  | Midi_note_label of { note : int }
  | Generated of string

type field = {
  key : string;
  label : string;
  change : Output_types.change_type;
  domain_type : Output_types.domain_type;
  old_value : scalar option;
  new_value : scalar option;
}

type node = {
  key : string;
  label : label;
  change : Output_types.change_type;
  domain_type : Output_types.domain_type;
  nesting : nesting;
  fields : field list;
  children : node list;
}
