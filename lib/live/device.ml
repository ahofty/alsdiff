open Alsdiff_base
open Alsdiff_base.Diff


exception Not_implemented of string

type enum_desc = {
  min : int;
  max : int;
  enums : string array;
} [@@deriving eq]

type param_value =
  | Float of float
  | Int of int
  | Bool of bool
  | Enum of int * enum_desc
[@@deriving eq]


(* ================== Helper functions ================== *)

(** Extract a sub-path by dropping elements from both beginning and end.

    @param drop_begin Number of elements to drop from the beginning (default: 0)
    @param drop_end Number of elements to drop from the end (default: 0)
    @param path The input path string
    @return The sub-path with specified elements removed
*)
let sub_path ?(drop_begin = 0) ?(drop_end = 0) (path : string) : string =
  let path_parts = String.split_on_char '/' path in
  let non_empty_parts = List.filter (fun s -> s <> "") path_parts in
  let length = List.length non_empty_parts in

  (* Validate drop parameters *)
  if drop_begin < 0 then
    invalid_arg (Printf.sprintf "sub_path: drop_begin must be non-negative: %d" drop_begin)
  else if drop_end < 0 then
    invalid_arg (Printf.sprintf "sub_path: drop_end must be non-negative: %d" drop_end)
  else if drop_begin + drop_end > length then
    invalid_arg (Printf.sprintf "sub_path: cannot drop more elements than available: drop_begin:%d + drop_end:%d > length:%d"
                   drop_begin
                   drop_end
                   length);

  (* Calculate the range to keep *)
  let keep_start = drop_begin in
  let keep_end = length - drop_end in

  let sub_parts =
    non_empty_parts
    |> List.drop keep_start
    |> List.take (keep_end - keep_start)
  in

  (* Reconstruct the path *)
  match sub_parts with
  | [] -> ""
  | _ -> "/" ^ String.concat "/" sub_parts

(** [param_name_from_path path] extracts a parameter name from a path string.
    The path is relative to the device XML element. This function removes the
    first path component (device name) and joins the rest with "/".

    Examples:
    - "DeviceName/Param" -> "Param"
    - "DeviceName/Nested/Param" -> "Nested/Param"
    - "Param" -> "Param"
    - "" -> ""
*)
(* NOTE: This function was keep because all XML files like
   tests/compressor.xml that made for unit testing each individual module.
   In a full .als XML file, extract parameter name from a path doesn't need such complex logic.
   But in the other hand, unit testing against a full .als XML file is *SLOW*.
*)
let param_name_from_path (path : string) : string =
  let path_parts = String.split_on_char '/' path in
  let non_empty_parts = List.filter (fun s -> s <> "") path_parts in
  match non_empty_parts with
  | [] -> ""
  | [single] -> single (* Only one part, return it as-is *)
  | _ :: rest -> String.concat "/" rest (* Skip first part, join rest *)


(* ================== Common modules ================== *)
module MIDIMapping = struct
  type mapping_kind = Continuous | OnOff [@@deriving eq]

  type t = {
    target : int;               (* NoteOrController *) [@id.id] [@patch.skip]
    channel : int;              (* 0-15 for MIDI, 16 for Macro *)
    kind : mapping_kind;        [@patch.skip]
    low : int;
    high : int;
    (* TODO: MIDI Note mapping *)
  } [@@deriving eq, id, patch] [@@patch.generate_diff]

  let is_midi m = m.channel >= 0 && m.channel <= 15 (* MIDI Channel starts from 0 to 15 in Ableton Live .als XML *)
  let is_macro m = m.channel = 16

  (** Extract range from MidiControllerRange (continuous parameters) *)
  let extract_continuous_range (xml : Xml.t) : (int * int) option =
    match
      (Upath.get_int_attr_opt "/MidiControllerRange/Min" "Value" xml,
       Upath.get_int_attr_opt "/MidiControllerRange/Max" "Value" xml)
    with
    | (Some min_val, Some max_val) ->
      Some (min_val, max_val)
    | _ -> None

  (** Extract range from MidiCCOnOffThresholds (On/Off parameters) *)
  let extract_onoff_range (xml : Xml.t) : (int * int) option =
    match
      (Upath.get_int_attr_opt "/MidiCCOnOffThresholds/Min" "Value" xml,
       Upath.get_int_attr_opt "/MidiCCOnOffThresholds/Max" "Value" xml)
    with
    | (Some min_val, Some max_val) -> Some (min_val, max_val)
    | _ -> None

  (** [has_macro_mapping xml] checks if an XML elements has a macro mapping *)
  let has_macro_mapping (xml : Xml.t) : bool =
    let keymidi_xml = Upath.find "/KeyMidi" xml |> snd in
    let is_note = Upath.get_bool_attr "IsNote" "Value" keymidi_xml in
    let controller_map_mode = Upath.get_int_attr "ControllerMapMode" "Value" keymidi_xml in
    is_note && controller_map_mode <> 0

  let create (xml : Xml.t) : t =
    if not (has_macro_mapping xml) then
      raise (Xml.Xml_error (xml, "It's not a MIDIMapping"))
    else
      let target = Upath.get_int_attr "/KeyMidi/NoteOrController" "Value" xml in
      let channel = Upath.get_int_attr "/KeyMidi/Channel" "Value" xml in
      let continuous = extract_continuous_range xml in
      let onoff = extract_onoff_range xml in
      let (low, high, kind) = match (continuous, onoff) with
        | Some (l, h), None -> (l, h, Continuous)
        | None, Some (l, h) -> (l, h, OnOff)
        | _ -> raise (Xml.Xml_error (xml, "Invalid XML for creating a MIDIMapping"))
      in
      { target; channel; kind; low; high }

  let create_opt (xml : Xml.t) : t option = try Some (create xml) with _ -> None

  (** [create_head_key_midi xml] create a [MIDIMapping] from a [<HeadKeyMidi></HeadKeyMidi>] element *)
  let create_head_key_midi (xml : Xml.t) : t =
    let target = Upath.get_int_attr "/NoteOrController" "Value" xml in
    let channel = Upath.get_int_attr "/Channel" "Value" xml in
    (* NOTE: This function was only used for creating a MIDIMapping.t for Solo in the Mixer. Solo only supports on/off mapping *)
    { target; channel; kind = OnOff; low = 64; high = 127; }
end

module GenericParam = struct
  type t = {
    name : string;              [@patch.identity]
    value : param_value;
    automation : int;
    modulation : int;           (* parameter cannot modulated will be set to a negative number *)
    mapping : MIDIMapping.t option;  [@patch.skip]
  } [@@deriving eq, patch] [@@patch.generate_diff]

  let create ~parse_value xml =
    let name = Xml.get_name xml in
    let value = parse_value xml in
    let automation = Upath.get_int_attr_opt "/AutomationTarget" "Id" xml
      |> Option.value ~default:0 in
    let modulation = Upath.get_int_attr_opt "/ModulationTarget" "Id" xml
      |> Option.value ~default:0 in
    let mapping = MIDIMapping.create_opt xml in
    { name; value; automation; modulation; mapping }

  let create_int_manual xml =
    create xml ~parse_value:(fun x -> Int (Upath.get_int_attr "/Manual" "Value" x))

  let create_float_manual xml =
    create xml ~parse_value:(fun x -> Float (Upath.get_float_attr "/Manual" "Value" x))

  let create_bool_manual xml =
    create xml ~parse_value:(fun x -> Bool (Upath.get_bool_attr "/Manual" "Value" x))

  module ViewSpec(B : Alsdiff_view_spec_types.View_spec_types.S) = struct
    let pv_to_fv = function
      | Float f -> B.float_value f
      | Int i -> B.int_value i
      | Bool b -> B.bool_value b
      | Enum (e, _) -> B.int_value e

    let field_specs = [
      B.make_spec pv_to_fv "Value"
        (fun (v : t) -> v.value) (fun (p : Patch.t) -> p.value);
      B.make_int "Automation"
        (fun (v : t) -> v.automation) (fun (p : Patch.t) -> p.automation);
      B.make_int "Modulation"
        (fun (v : t) -> v.modulation) (fun (p : Patch.t) -> p.modulation);
    ]

    let section_specs = [
      B.Spec.inline_fields ~specs:field_specs
        ~domain_type:(B.domain_type_of_name "DTDefault")
    ]

    let build_value_fields ?(domain_type = B.domain_type_of_name "DTDefault") ct v =
      B.build_value_field_views field_specs ct v ~domain_type

    let build_patch_fields ?(domain_type = B.domain_type_of_name "DTDefault") p =
      B.build_patch_field_views field_specs p ~domain_type
  end
end


module NameIdGenericParam = struct
  include GenericParam

  let has_same_id a b = a.name = b.name
  let id_hash t = Hashtbl.hash t.name
end


module DeviceParam = struct
  type t = {
    base : GenericParam.t;  [@view.inline_child]
  } [@@deriving eq, patch, view_spec] [@@patch.generate_diff]

  let has_same_id a b = a.base.name = b.base.name
  let id_hash t = Hashtbl.hash t.base.name

  (** [create path xml] creates a device parameter from a Device XML element.
      It raises [Failure "Invalid XML element for creating DeviceParam"] if the XML
      is not a valid element, and raises [Failure "Failed to parse device parameter"]
      if the parameter value cannot be parsed.

      @param path the path relative to the device XML element
      @param xml the device XML element
  *)
  let create (path : string) (xml : Xml.t) : t =
    match xml with
    | Xml.Element _ ->
      let name = param_name_from_path path in
      let parse_value = fun xml ->
        let literal = Upath.get_attr "/Manual" "Value" xml in
        match literal with
        | "true" | "false" -> Bool (bool_of_string literal)
        | _ -> Float (float_of_string literal)
      in
      let base = GenericParam.create ~parse_value xml in
      let name_updated_base = { base with name } in
      { base = name_updated_base }
    | _ -> raise (Xml.Xml_error (xml, "Invalid XML element for creating DeviceParam"))

  let create_from_upath_find (path, xml) = create path xml
end


module PresetRef = struct
  type preset_type =
    | UserPreset
    | DefaultPreset
  [@@deriving eq]

  type t = {
    id : int;                   (* not unique *) [@id.id] [@patch.skip]
    name : string;              [@patch.skip] [@view.const]
    preset_type : preset_type;  [@patch.skip]
    relative_path : string;
    path : string;              [@view.skip]
    pack_name : string;
    pack_id : int;              [@view.skip]
    file_size : int;            [@view.skip]
    crc : int;                  [@view.skip]
  } [@@deriving eq, id, patch, view_spec] [@@patch.generate_diff]

  let create (xml : Xml.t) : t =
    match xml with
    | Xml.Element { name=tag; childs; _ } ->
      let id = Xml.get_int_attr "Id" xml in
      let preset_type =
        match tag with
        | "FilePresetRef" -> UserPreset
        | "AbletonDefaultPresetRef" -> DefaultPreset
        | _ -> raise (Xml.Xml_error (xml, "Unknown PresetRef type" ^ tag))
      in

      (* Extract FileRef element *)
      let file_ref_xml =
        try List.find (function
            | Xml.Element { name = "FileRef"; _ } -> true
            | _ -> false) childs
        with Not_found -> raise (Xml.Xml_error (xml, "FileRef element not found in PresetRef"))
      in

      (* Extract all the required fields from FileRef *)
      let relative_path = Upath.get_attr "/RelativePath" "Value" file_ref_xml in
      let path = Upath.get_attr "/Path" "Value" file_ref_xml in

      let preset_file_name = Upath.get_attr "/FileRef/Path" "Value" xml |> Filename.basename |> Filename.remove_extension in
      let name = match preset_type with
        | UserPreset -> preset_file_name
        | DefaultPreset ->
          let device_name = Upath.get_attr "/DeviceId" "Name" xml in
          if device_name <> "" then device_name else preset_file_name
      in

      let pack_name = Upath.get_attr "/LivePackName" "Value" file_ref_xml in
      let pack_id = Upath.get_int_attr_opt "/LivePackId" "Value" file_ref_xml |> Option.value ~default:0 in
      let file_size = Upath.get_int_attr "/OriginalFileSize" "Value" file_ref_xml in
      let crc = Upath.get_int_attr_opt "/OriginalCrc" "Value" file_ref_xml |> Option.value ~default:0 in

      { id; name; preset_type; relative_path; path; pack_name; pack_id; file_size; crc }
    | _ -> raise (Xml.Xml_error (xml, "Invalid XML element for creating PresetRef"))
end


(* ================== M4L PatchRef module ================== *)
module PatchRef = struct
  type t = {
    id : int;                              [@id.id] [@patch.skip]
    name : string;                         [@patch.skip] [@view.const]
    preset_type : PresetRef.preset_type;   [@patch.skip]
    relative_path : string;
    path : string;                         [@view.skip]
    pack_name : string;
    pack_id : int;                         [@view.skip]
    file_size : int;                       [@view.skip]
    crc : int;                             [@view.skip]
    last_mod_date : int;  (* LastModDate Value attribute - UNIX timestamp *) [@view.scalar unix_timestamp] [@view.label "Last Modified"]
  } [@@deriving eq, id, patch, view_spec] [@@patch.generate_diff]

  let create (xml : Xml.t) : t =
    match xml with
    | Xml.Element { name; childs; _ } when name = "MxPatchRef" || name = "MxDPatchRef" ->
      let id = Xml.get_int_attr "Id" xml in
      let preset_type = PresetRef.UserPreset in (* M4L patches are always user presets *)

      (* Extract FileRef element *)
      let file_ref_xml =
        try List.find (function
            | Xml.Element { name = "FileRef"; _ } -> true
            | _ -> false) childs
        with Not_found -> raise (Xml.Xml_error (xml, "FileRef element not found in MxPatchRef"))
      in

      (* Extract all the required fields from FileRef *)
      let relative_path = Upath.get_attr "/RelativePath" "Value" file_ref_xml in
      let path = Upath.get_attr "/Path" "Value" file_ref_xml in
      let preset_file_name = Upath.get_attr "/Path" "Value" file_ref_xml |> Filename.basename |> Filename.remove_extension in

      (* Extract LastModDate as int UNIX timestamp *)
      let last_mod_date =
        try Upath.get_attr "/LastModDate" "Value" xml |> int_of_string
        with _ -> 0 (* Default to 0 if not found *)
      in

      let name = preset_file_name in
      let pack_name = Upath.get_attr "/LivePackName" "Value" file_ref_xml in
      let pack_id = Upath.get_int_attr_opt "/LivePackId" "Value" file_ref_xml |> Option.value ~default:0 in
      let file_size = Upath.get_int_attr "/OriginalFileSize" "Value" file_ref_xml in
      let crc = Upath.get_int_attr_opt "/OriginalCrc" "Value" file_ref_xml |> Option.value ~default:0 in

      { id; name; preset_type; relative_path; path; pack_name; pack_id; file_size; crc; last_mod_date }
    | _ -> raise (Xml.Xml_error (xml, "Invalid XML element for creating PatchRef (expected MxPatchRef)"))
end


(* ================== Plugin related modules ================== *)
module PluginParam = struct
  type t = {
    id : int;
    base : GenericParam.t;
  } [@@deriving eq]

  let create (xml : Xml.t) : t =
    let id = Xml.get_int_attr "Id" xml in

    let param_type = Xml.get_name xml in
    let parse_value val_xml =
      match param_type with
      | "PluginFloatParameter" ->
        Float (Upath.get_float_attr "/Manual" "Value" val_xml)
      | "PluginIntParameter" ->
        Int (Upath.get_int_attr "/Manual" "Value" val_xml)
      | "PluginBoolParameter" ->
        Bool (Upath.get_bool_attr "/Manual" "Value" val_xml)
      | "PluginEnumParameter" ->
        (* raise (Not_implemented "PluginEnumParameter parsing is not yet implemented. If you encounter this error, please consider sharing your .als file to help improve support for this feature.") *)
        Int (Upath.get_int_attr "/Manual" "Value" val_xml)
      | _ -> raise (Xml.Xml_error (xml, "Invalid parameter type " ^ param_type))
    in
    let base = Upath.find "/ParameterValue" xml |> snd |> GenericParam.create ~parse_value in
    let name = Upath.get_attr "/ParameterName" "Value" xml in
    let name_updated_base = { base with name } in
    { id; base = name_updated_base; }

  module Patch = struct
    type t = {
      id : int;
      base : GenericParam.Patch.t structured_update;
    }

    let is_empty p = is_unchanged_update (module GenericParam.Patch) p.base
  end

  (* NOTE: From my observation, ParameterId seems always equal to -1, which is useless for an identifier.
     We use the Id attribute as the identifier for PluginParam since it's unique and stable *)
  let has_same_id a b = a.id = b.id

  let id_hash t = Hashtbl.hash t.id

  let diff (old_param : t) (new_param : t) : Patch.t =
    if not (has_same_id old_param new_param) then
      failwith "cannot diff two PluginParams with different identifiers"
    else
      let module PluginSpecializedGenericParam = struct
        include GenericParam

        let has_same_id _ _ = true
        let id_hash _ = 0
      end in

      let base_change = diff_complex_value_id (module PluginSpecializedGenericParam) old_param.base new_param.base in
      { id = new_param.id; base = base_change }

  module ViewSpec(B : Alsdiff_view_spec_types.View_spec_types.S) = struct
    let __inline_base =
      let module Vs = (GenericParam.ViewSpec)(B) in
      B.map_specs (fun (v : t) -> v.base) (fun (p : Patch.t) -> p.base) Vs.field_specs
    let field_specs = List.append [] __inline_base
    let section_specs = [
      B.Spec.inline_fields ~specs:field_specs ~domain_type:(B.domain_type_of_name "DTDefault")
    ]
    let build_value_fields ?(domain_type = B.domain_type_of_name "DTDefault") ct v =
      B.build_value_field_views field_specs ct v ~domain_type
    let build_patch_fields ?(domain_type = B.domain_type_of_name "DTDefault") p =
      B.build_patch_field_views field_specs p ~domain_type
    let build_item ?(name = "") ?(domain_type = B.domain_type_of_name "DTDefault") c =
      B.build_item_from_specs ~name ~domain_type ~specs:section_specs c
  end
end


module PluginDesc = struct
  type plugin_type = Vst2 | Vst3 | Auv2 [@@deriving eq]

  module PluginTypeEq = Equality.MakeDefaultEq(struct type t = plugin_type end)

  type t = {
    name : string;
    uid : string;
    plugin_type : plugin_type;
    processor_state : string;
    (* NOTE: Since the controller state in VST3 is the state of the GUI, which has
       nothing to do with actual audio/dsp processing, we just ignore it for god's sake *)
    (* controller_state : string; *)
  } [@@deriving eq]

  let plugin_type_to_string = function
    | Vst2 -> "VST2"
    | Vst3 -> "VST3"
    | Auv2 -> "AUv2"

  let parse_vst3_uid xml =
    let fields = Upath.find_all "/'Fields\\.[0-9]+$'" xml in
    let sorted_fields = List.sort (fun (a, _) (b, _) -> String.compare a b) fields in
    sorted_fields
    |> List.map (fun (_, field_xml) -> Xml.get_attr "Value" field_xml)
    |> String.concat "-"

  let re_whitespaces = Re.compile (Re.Pcre.re "\\s+")
  let reformat_blob_str s =
    Re.replace_string ~all:true re_whitespaces ~by:"" s

  let parse_vst3_processor_state plugin_info_xml =
    Upath.find_opt "/Preset/Vst3Preset/ProcessorState" plugin_info_xml
    |> Option.map (fun x -> snd x |> Xml.get_data_child |> reformat_blob_str)
    |> Option.value ~default:""

  let parse_vst3_info plugin_info_xml =
    (* Get plugin name - try different possible locations *)
    let name =
      match Upath.get_attr_opt "/Name" "Value" plugin_info_xml with
      | Some value -> value
      | None ->
        match Upath.get_attr_opt "/PlugName" "Value" plugin_info_xml with
        | Some plug_name -> plug_name
        | None -> "Unknown VST3 Plugin"
    in

    (* Get UID from VST3 specific structure *)
    let uid = Upath.find "/Uid" plugin_info_xml |> snd |> parse_vst3_uid in

    (* Get processor state from VST3 preset *)
    let processor_state = parse_vst3_processor_state plugin_info_xml in
    (name, uid, processor_state)

  let parse_vst2_processor_state plugin_info_xml =
    (* VST2 plugins typically don't have processor state in the same way as VST3 *)
    Upath.find_opt "/Preset/VstPreset/State" plugin_info_xml
    |> Option.map (fun x -> snd x |> Xml.get_data_child |> reformat_blob_str)
    |> Option.value ~default:""

  let parse_vst2_info plugin_info_xml =
    (* Get plugin name - try different possible locations for VST2 *)
    let name =
      match Upath.get_attr_opt "/PlugName" "Value" plugin_info_xml with
      | Some plug_name -> plug_name
      | None ->
        match Upath.get_attr_opt "/Name" "Value" plugin_info_xml with
        | Some value -> value
        | None -> "Unknown VST2 Plugin"
    in

    (* Get UID from VST2 specific structure *)
    let uid =
      match Upath.get_attr_opt "/UniqueId" "Value" plugin_info_xml with
      | Some unique_id -> unique_id
      | None ->
        (* Try to extract from path as fallback *)
        match Upath.get_attr_opt "/Path" "Value" plugin_info_xml with
        | Some path ->
          (* Extract plugin name from path and create a simple hash *)
          let filename = Filename.basename path in
          string_of_int (Hashtbl.hash filename)
        | None -> "0"
    in

    (* Get processor state from VST2 preset *)
    let processor_state = parse_vst2_processor_state plugin_info_xml in

    (name, uid, processor_state)

  let parse_au_processor_state plugin_info_xml =
    (* AU plugins use Buffer element for state data *)
    Upath.find_opt "/Preset/AuPreset/Buffer" plugin_info_xml
    |> Option.map (fun x -> snd x |> Xml.get_data_child |> reformat_blob_str)
    |> Option.value ~default:""

  let parse_au_info plugin_info_xml =
    (* Get plugin name for AU plugins *)
    let name =
      match Upath.get_attr_opt "/Name" "Value" plugin_info_xml with
      | Some value -> value
      | None -> "Unknown AU Plugin"
    in

    (* Create UID from AU component identifiers *)
    let uid =
      match Upath.get_attr_opt "/ComponentType" "Value" plugin_info_xml,
            Upath.get_attr_opt "/ComponentSubType" "Value" plugin_info_xml,
            Upath.get_attr_opt "/ComponentManufacturer" "Value" plugin_info_xml with
      | Some comp_type, Some comp_subtype, Some comp_manufacturer ->
        Printf.sprintf "%s-%s-%s" comp_type comp_subtype comp_manufacturer
      | _ ->
        (* Fallback: try to use manufacturer and name *)
        match Upath.get_attr_opt "/Manufacturer" "Value" plugin_info_xml with
        | Some manufacturer ->
          let combined = manufacturer ^ "-" ^ name in
          string_of_int (Hashtbl.hash combined)
        | None ->
          (* Last resort: hash the plugin name *)
          string_of_int (Hashtbl.hash name)
    in

    (* Get processor state from AU preset buffer *)
    let processor_state = parse_au_processor_state plugin_info_xml in

    (name, uid, processor_state)

  let create (xml : Xml.t) : t =
    (* Extract plugin type based on the element name *)
    let plugin_info_xml = Xml.get_childs xml |> List.hd in

    let plugin_type =
      match Xml.get_name plugin_info_xml with
      | "Vst3PluginInfo" -> Vst3
      | "VstPluginInfo" -> Vst2
      | "AuPluginInfo" -> Auv2
      | name -> raise (Xml.Xml_error (plugin_info_xml, "Unsupported plugin type: " ^ name))
    in

    let (name, uid, processor_state) =
      match plugin_type with
      | Vst3 -> parse_vst3_info plugin_info_xml
      | Vst2 -> parse_vst2_info plugin_info_xml
      | Auv2 -> parse_au_info plugin_info_xml
    in

    { name; uid; plugin_type; processor_state }

  module Patch = struct
    type t = {
      name : string atomic_update;
      uid : string atomic_update;
      plugin_type : plugin_type atomic_update;
      state : string atomic_update;
    }

    let is_empty p =
      is_unchanged_atomic_update p.name &&
      is_unchanged_atomic_update p.uid &&
      is_unchanged_atomic_update p.plugin_type &&
      is_unchanged_atomic_update p.state
  end

  let diff (old_desc : t) (new_desc : t) : Patch.t =
    if old_desc.uid <> new_desc.uid then
      failwith "cannot diff two PluginDesc with different UIDs"
    else
      let name_change = diff_atomic_value (module String) old_desc.name new_desc.name in
      let uid_change = diff_atomic_value (module String) old_desc.uid new_desc.uid in
      let plugin_type_change = diff_atomic_value (module PluginTypeEq) old_desc.plugin_type new_desc.plugin_type in
      let state_change = diff_atomic_value (module String) old_desc.processor_state new_desc.processor_state in
      {
        Patch.name = name_change;
        uid = uid_change;
        plugin_type = plugin_type_change;
        state = state_change;
      }

  let has_same_id a b = a.uid = b.uid

  let id_hash t = Hashtbl.hash t.uid

  module ViewSpec(B : Alsdiff_view_spec_types.View_spec_types.S) = struct
    let field_specs = [
      B.make_string "Name" (fun (d : t) -> d.name) (fun (p : Patch.t) -> p.name);
      B.make_string "UID" (fun (d : t) -> d.uid) (fun (p : Patch.t) -> p.uid);
      B.make_spec (fun (pt : plugin_type) -> B.string_value (plugin_type_to_string pt))
        "Type" (fun (d : t) -> d.plugin_type) (fun (p : Patch.t) -> p.plugin_type);
      B.make_spec (fun (_ : string) -> B.string_value "<state>")
        "Processor State" (fun (d : t) -> d.processor_state) (fun (p : Patch.t) -> p.state);
    ]
    let section_specs = [
      B.Spec.inline_fields ~specs:field_specs ~domain_type:(B.domain_type_of_name "DTDevice")
    ]
    let build_value_fields ?(domain_type = B.domain_type_of_name "DTDevice") ct v =
      B.build_value_field_views field_specs ct v ~domain_type
    let build_patch_fields ?(domain_type = B.domain_type_of_name "DTDevice") p =
      B.build_patch_field_views field_specs p ~domain_type
    let build_item ?(name = "") ?(domain_type = B.domain_type_of_name "DTDevice") c =
      B.build_item_from_specs ~name ~domain_type ~specs:section_specs c
  end
end


(* ================== Max4Live device related modules ================== *)
module Max4LiveParam = struct
  type t = {
    id : int;                   (* ParameterId *) [@id.id] [@patch.skip]
    index : int;                (* VisualIndex *)
    base : GenericParam.t;  [@view.inline_child]
  } [@@deriving eq, id, patch, view_spec] [@@patch.generate_diff]

  (** Extract enum description from Names/Name/Name structure - specific to M4L *)
  let extract_enum_desc (xml : Xml.t) : enum_desc option =
    try
      (* Find all Name elements under Names *)
      let name_elements = Upath.find_all "/Names/Name/Name" xml in
      let enums =
        List.map (fun (_, name_xml) ->
            Xml.get_attr "Value" name_xml
          ) name_elements
        |> Array.of_list
      in
      if Array.length enums > 0 then
        let max_id = Array.length enums - 1 in
        Some { min = 0; max = max_id; enums }
      else
        None
    with _ -> None

  (** Create M4L parameter from XML element *)
  let create (_path : string) (xml : Xml.t) : t =
    let id = Xml.get_int_attr "Id" xml in
    let index = Upath.get_int_attr "/Index" "Value" xml in

    let param_type = Xml.get_name xml in
    let parse_value = fun val_xml ->
      match param_type with
      | "MxDFloatParameter" ->
        Float (Upath.get_float_attr "/Manual" "Value" val_xml)
      | "MxDIntParameter" ->
        Int (Upath.get_int_attr "/Manual" "Value" val_xml)
      | "MxDBoolParameter" ->
        Bool (Upath.get_bool_attr "/Manual" "Value" val_xml)
      | "MxDEnumParameter" ->
        let enum_value = Upath.get_int_attr "/Manual" "Value" val_xml in
        let enum_desc_opt = extract_enum_desc xml in
        (match enum_desc_opt with
         | Some enum_desc  -> Enum (enum_value, enum_desc)
         | None -> raise (Xml.Xml_error (xml, "Haven't found enum definitions")))
      | _ -> raise (Xml.Xml_error (xml, "Invalid M4L parameter type: " ^ param_type))
    in

    let name = Upath.get_attr "/Name" "Value" xml in
    let base = Upath.find "/Timeable" xml |> snd |> GenericParam.create ~parse_value in
    let name_updated_base = { base with name } in
    { id; index; base = name_updated_base; }

  let create_from_upath_find (path, xml) = create path xml
end


(* ================== Group device related modules ================== *)
(* The rack chain's mixer is different to track's mixer *)
module MixerDevice = struct
  type t = {
    on : DeviceParam.t;         [@view.child "DTParam"] [@view.label "On"]
    speaker : DeviceParam.t;    [@view.child "DTParam"] [@view.label "Speaker"]
    volume : DeviceParam.t option;     [@view.optional_child "DTParam"] [@view.label "Volume"]
    pan : DeviceParam.t option;        [@view.optional_child "DTParam"] [@view.label "Pan"]
  } [@@deriving eq, patch, view_spec] [@@patch.generate_diff]

  let create (xml : Xml.t) : t =
    match xml with
    | Xml.Element { name = "MixerDevice"; _ } ->
      (* Extract On parameter *)
      let on = Upath.find "/On" xml |> DeviceParam.create_from_upath_find in
      let speaker = Upath.find "/Speaker" xml |> DeviceParam.create_from_upath_find in
      let volume = Upath.find_opt "/Volume" xml |> Option.map DeviceParam.create_from_upath_find in
      let pan = Upath.find_opt "/Panorama" xml |> Option.map DeviceParam.create_from_upath_find in
      { on; speaker; volume; pan }
    | _ -> raise (Xml.Xml_error (xml, "Invalid XML element for creating MixerDevice"))

  (* MixerDevice doesn't have a natural ID, so use placeholder interface *)
  let has_same_id _ _ = true
  let id_hash _ = Hashtbl.hash 0
end


(** [extract_index_from_name element_name] extracts the numeric index from
    element names like "MacroValues.3", "MacroControls.15", etc.
    Raises [Failure] if the element name doesn't contain a valid index. *)
let extract_index_from_name (element_name : string) : int =
  let parts = String.split_on_char '.' element_name in
  match List.rev parts with
  | index :: _ -> int_of_string index
  | [] -> failwith ("Invalid element name: " ^ element_name)


module Macro = struct
  type t = {
    id : int;               [@id.id] [@patch.skip]
    base : GenericParam.t;  [@view.inline_child]
  } [@@deriving eq, id, patch, view_spec] [@@patch.generate_diff]

  let create (name_xml : Xml.t) (control_xml : Xml.t) : t =
    (* Extract the macro name from MacroDisplayNames element *)
    let name_id = extract_index_from_name @@ Xml.get_name name_xml in
    let control_id = extract_index_from_name @@ Xml.get_name control_xml in
    let base = GenericParam.create_float_manual control_xml in
    if name_id <> control_id then
      raise (Xml.Xml_error (name_xml, "Macro name ID " ^ string_of_int name_id ^ " does not match control ID " ^ string_of_int control_id ^ ". Macro names and controls must be paired correctly."))
    else
      { id=name_id; base; }
end


module Snapshot = struct
  type t = {
    id : int;               [@id.id] [@patch.skip] [@view.skip]
    name : string;
    values : float list;    [@view.skip]
  } [@@deriving eq, id, patch, view_spec]

  let create (xml : Xml.t) : t =
    match xml with
    | Xml.Element { name = "MacroSnapshot"; _ } ->
      let id = Xml.get_int_attr "Id" xml in
      let name = Upath.get_attr "/SnapshotName" "Value" xml in

      (* Extract all MacroValues.N elements using regex *)
      let macro_values_xml = Upath.find_all "/'MacroValues\\.[0-9]+'" xml in

      (* Build ordered list of values from MacroValues *)
      let values =
        List.map (fun (_, xml_elem) ->
            let element_name = match xml_elem with
              | Xml.Element { name; _ } -> name
              | Xml.Data _ -> raise (Xml.Xml_error (xml_elem, "Expected Element, got Data"))
            in
            let index = extract_index_from_name element_name in
            let value = Xml.get_float_attr "Value" xml_elem in
            (index, value)
          ) macro_values_xml
        |> List.sort (fun (i1, _) (i2, _) -> Stdlib.compare i1 i2)
        |> List.map snd
      in

      { id; name; values }
    | _ -> raise (Xml.Xml_error (xml, "Invalid XML element for creating Snapshot"))

  let diff (old_snapshot : t) (new_snapshot : t) : Patch.t =
    if old_snapshot.id <> new_snapshot.id then
      failwith "cannot diff two Snapshots with different Ids"
    else
      let name_change = diff_atomic_value (module String) old_snapshot.name new_snapshot.name in
      let values_changes =
        if List.length old_snapshot.values <> List.length new_snapshot.values then
          failwith "diff_atomic_list requires lists of same length"
        else
          List.map2 (fun old_elem new_elem ->
              (diff_atomic_value (module Stdlib.Float) old_elem new_elem :> float atomic_change)
            ) old_snapshot.values new_snapshot.values
      in
      { name = name_change; values = values_changes }
end


(* ================== Type definitions ================== *)
(* JEEZ, CIRCULR DEPENDENCIES *)
type device =
  | Regular of regular_device
  | Plugin of plugin_device
  | Max4Live of max4live_device
  | Group of group_device [@@deriving eq]

and regular_device = {
  id : int;
  device_name : string;
  display_name : string;        (* either UserName or PresetName *)
  pointee : int;
  enabled : DeviceParam.t;
  params : DeviceParam.t list;
  preset : PresetRef.t option;
} [@@deriving eq]

and plugin_device = {
  id : int;
  device_name : string;
  display_name : string;
  pointee : int;
  enabled : DeviceParam.t;
  desc : PluginDesc.t;
  params : PluginParam.t list;
  preset : PresetRef.t option;
  (* TODO: Support sidechain and MPE settigns *)
} [@@deriving eq]

and max4live_device = {
  id : int;
  device_name : string;
  display_name : string;
  pointee : int;
  enabled : DeviceParam.t;
  patch_ref : PatchRef.t;       (* the .amxd file *)
  params : Max4LiveParam.t list;
  preset : PresetRef.t option;
} [@@deriving eq]

and branch = {
  id : int;
  devices : device list;
  mixer : MixerDevice.t;
} [@@deriving eq]
and group_device = {
  id : int;
  device_name : string;
  display_name : string;
  pointee : int;
  enabled : DeviceParam.t;
  branches : branch list;
  macros : Macro.t list;
  snapshots : Snapshot.t list;
  preset : PresetRef.t option;
} [@@deriving eq]

type device_patch =
  | RegularPatch of regular_device_patch
  | PluginPatch of plugin_device_patch
  | Max4LivePatch of max4live_device_patch
  | GroupPatch of group_device_patch

and regular_device_patch = {
  id : int;                    (* immutable identity *)
  device_name : string;         (* immutable identity *)
  display_name : string atomic_update;

  (* parameters can be added or removed due to Ableton updates on their built-in devices,
     so change is more semantically correct than update *)
  params : (DeviceParam.t, DeviceParam.Patch.t) structured_change list;

  preset : (PresetRef.t, PresetRef.Patch.t) structured_change;
}

and plugin_device_patch = {
  id : int;                    (* immutable identity *)
  device_name : string;         (* immutable identity *)
  display_name : string atomic_update;
  enabled : DeviceParam.Patch.t structured_update;
  desc : PluginDesc.Patch.t structured_update;
  params : (PluginParam.t, PluginParam.Patch.t) structured_change list;
  preset : (PresetRef.t, PresetRef.Patch.t) structured_change;
}

and max4live_device_patch = {
  id : int;                    (* immutable identity *)
  device_name : string;         (* immutable identity *)
  display_name : string atomic_update;
  enabled : DeviceParam.Patch.t structured_update;
  patch_ref : (PatchRef.t, PatchRef.Patch.t) structured_change;
  params : (Max4LiveParam.t, Max4LiveParam.Patch.t) structured_change list;
  preset : (PresetRef.t, PresetRef.Patch.t) structured_change;
}

and branch_patch = {
  id : int atomic_update;
  devices : (device, device_patch) structured_change list;
  mixer : MixerDevice.Patch.t structured_update;
}
and group_device_patch = {
  id : int;                    (* immutable identity *)
  device_name : string;         (* immutable identity *)
  display_name : string atomic_update;
  enabled : DeviceParam.Patch.t structured_update;

  (* devices always have preset, its either user-defined one or the defualt one,
     so only Unchanged/Modified cases *)
  branches : (branch, branch_patch) structured_change list;
  macros : (Macro.t, Macro.Patch.t) structured_change list;
  snapshots : (Snapshot.t, Snapshot.Patch.t) structured_change list;
  preset : (PresetRef.t, PresetRef.Patch.t) structured_change;
}


(* ================== Forward Reference Declarations ================== *)
(* These mutable references are used to break the circular dependency between
   device, branch patch is_empty functions and the diff functions. They are
   initialized at the end of the file after all types and modules are defined. *)
let device_patch_is_empty_ref : (device_patch -> bool) ref =
  ref (fun _ -> failwith "device_patch_is_empty not initialized")
let branch_patch_is_empty_ref : (branch_patch -> bool) ref =
  ref (fun _ -> failwith "branch_patch_is_empty not initialized")

(* Helper function to get device type name and id for error messages *)
let get_device_type_and_id = function
  | Regular r -> "Regular", r.id
  | Plugin p -> "Plugin", p.id
  | Group g -> "Group", g.id
  | Max4Live m -> "Max4Live", m.id

(* regular_device diff functions *)
let rec regular_device_diff (old_device : regular_device) (new_device : regular_device) : regular_device_patch =
  if old_device.id <> new_device.id && old_device.device_name <> new_device.device_name  then
    failwith (Printf.sprintf "Cannot diff two RegularDevices with different Ids & Device names: %d/%s vs %d/%s"
                old_device.id old_device.device_name new_device.id new_device.device_name)
  else
    let display_name_change = diff_atomic_value (module String) old_device.display_name new_device.display_name in
    let preset_change = diff_complex_value_opt (module PresetRef) old_device.preset new_device.preset in
    let params_changes =
      diff_list_id (module DeviceParam) old_device.params new_device.params
      |> filter_changes (module DeviceParam.Patch)
    in
    {
      id = new_device.id;
      device_name = new_device.device_name;
      display_name = display_name_change;
      preset = preset_change;
      params = params_changes;
    }
(* plugin_device diff functions *)
and plugin_device_diff (old_device : plugin_device) (new_device : plugin_device) : plugin_device_patch =
  if old_device.id <> new_device.id then
    failwith (Printf.sprintf "Cannot diff two PluginDevices with different IDs: %d vs %d (Name: %s)"
                old_device.id new_device.id old_device.device_name)
  else
    let display_name_change =
      diff_atomic_value (module String) old_device.display_name new_device.display_name
    in
    let enabled_change =
      diff_complex_value (module DeviceParam) old_device.enabled new_device.enabled
    in

    let desc_change =
      diff_complex_value (module PluginDesc) old_device.desc new_device.desc
    in

    let params_change =
      diff_list_id (module PluginParam) old_device.params new_device.params
      |> filter_changes (module PluginParam.Patch)
    in

    let preset_change =
      diff_complex_value_opt (module PresetRef) old_device.preset new_device.preset
    in

    {
      id = new_device.id;
      device_name = new_device.device_name;
      display_name = display_name_change;
      enabled = enabled_change;
      desc = desc_change;
      params = params_change;
      preset = preset_change
    }

(* max4live_device diff functions *)
and max4live_device_diff (old_device : max4live_device) (new_device : max4live_device) : max4live_device_patch =
  if old_device.id <> new_device.id then
    failwith (Printf.sprintf "Cannot diff two Max4LiveDevices with different IDs: %d vs %d (Name: %s)"
                old_device.id new_device.id old_device.device_name)
  else
    let display_name_change =
      diff_atomic_value (module String) old_device.display_name new_device.display_name
    in
    let enabled_change =
      diff_complex_value (module DeviceParam) old_device.enabled new_device.enabled
    in
    let patch_ref_change =
      (diff_complex_value (module PatchRef) old_device.patch_ref new_device.patch_ref :> (PatchRef.t, PatchRef.Patch.t) structured_change)
    in
    let params_change =
      diff_list_id (module Max4LiveParam) old_device.params new_device.params
      |> filter_changes (module Max4LiveParam.Patch)
    in
    let preset_change =
      diff_complex_value_opt (module PresetRef) old_device.preset new_device.preset
    in
    {
      id = new_device.id;
      device_name = new_device.device_name;
      display_name = display_name_change;
      enabled = enabled_change;
      patch_ref = patch_ref_change;
      params = params_change;
      preset = preset_change
    }

(* group_device diff functions *)
and  branch_diff (old_branch : branch) (new_branch : branch) =
  if old_branch.id <> new_branch.id then
    failwith (Printf.sprintf "Cannot diff two Branches with different Ids: %d vs %d" old_branch.id new_branch.id)
  else
    let id_change = `Unchanged in (* IDs must be the same *)
    (* Minimal delegation module to avoid circular dependencies *)
    let module DeviceId = struct
      type t = device
      let equal = (=)
      let has_same_id a b =
        match a, b with
        | Regular ra, Regular rb -> ra.id = rb.id
        | Plugin pa, Plugin pb -> pa.id = pb.id
        | Group ga, Group gb -> ga.id = gb.id
        | Max4Live ma, Max4Live mb -> ma.id = mb.id
        | _ -> false
      let id_hash = function
        | Regular r -> Hashtbl.hash r.id
        | Plugin p -> Hashtbl.hash p.id
        | Group g -> Hashtbl.hash g.id
        | Max4Live m -> Hashtbl.hash m.id

      module Patch = struct
        type t = device_patch
        (* Use mutable references that will be initialized at module load time.
           The device_patch_is_empty_ref will be set after the mutually recursive
           helpers are defined at the end of the file. *)
        let is_empty p = !device_patch_is_empty_ref p
      end

      let diff old_dev new_dev =
        match old_dev, new_dev with
        | Regular ro, Regular rn -> RegularPatch (regular_device_diff ro rn)
        | Plugin po, Plugin pn -> PluginPatch (plugin_device_diff po pn)
        | Group go, Group gn -> GroupPatch (group_device_diff go gn)
        | Max4Live mo, Max4Live mn -> Max4LivePatch (max4live_device_diff mo mn)
        | _ ->
          let t1, id1 = get_device_type_and_id old_dev in
          let t2, id2 = get_device_type_and_id new_dev in
          failwith (Printf.sprintf "Cannot diff devices of different types: %s(Id=%d) vs %s(Id=%d)"
                      t1 id1 t2 id2)
    end in
    let devices_changes =
      diff_list_id (module DeviceId) old_branch.devices new_branch.devices
      |> filter_changes (module DeviceId.Patch)
    in
    let mixer_change = diff_complex_value_id (module MixerDevice) old_branch.mixer new_branch.mixer in
    {
      id = id_change;
      devices = devices_changes;
      mixer = mixer_change;
    }
and group_device_diff (old_group : group_device) (new_group : group_device) =
  if old_group.id <> new_group.id then
    failwith (Printf.sprintf "Cannot diff two GroupDevices with different Ids: %d vs %d (Name: %s)"
                old_group.id new_group.id old_group.device_name)
  else
    let display_name_change = diff_atomic_value (module String) old_group.display_name new_group.display_name in
    let enabled_change = diff_complex_value (module DeviceParam) old_group.enabled new_group.enabled in
    let preset_change = diff_complex_value_opt (module PresetRef) old_group.preset new_group.preset in
    let branches_changes =
      let module BranchId = struct
        type t = branch
        let equal = (=)
        let has_same_id (a : t) (b : t) = a.id = b.id
        let id_hash (t : t) = Hashtbl.hash t.id
        module Patch = struct
          type t = branch_patch
          (* Use mutable reference that will be initialized at module load time *)
          let is_empty p = !branch_patch_is_empty_ref p
        end
        let diff = branch_diff
      end in
      diff_list_id (module BranchId) old_group.branches new_group.branches
      |> filter_changes (module BranchId.Patch)
    in
    let macros_changes =
      diff_list_id (module Macro) old_group.macros new_group.macros
      |> filter_changes (module Macro.Patch)
    in
    let snapshots_changes =
      diff_list_id (module Snapshot) old_group.snapshots new_group.snapshots
      |> filter_changes (module Snapshot.Patch)
    in
    {
      id = new_group.id;
      device_name = new_group.device_name;
      display_name = display_name_change;
      enabled = enabled_change;
      preset = preset_change;
      branches = branches_changes;
      macros = macros_changes;
      snapshots = snapshots_changes;
    }


(** Get display name based on [ShouldShowPresetName].
    if [ShouldShowPresetName] set to be [true], return the preset name.
    else return [UserName] if exists, if [UserName] is empty,
    return device type name.
    For a device with default preset, return the [DeviceId] as display name
    instead device type name.

    @param preset The preset reference of the device
    @param xml The XML element of the device
*)
let get_display_name (preset_opt : PresetRef.t option) (xml : Xml.t) =
  if Upath.get_bool_attr "ShouldShowPresetName" "Value" xml &&
     Option.is_some preset_opt then
    Option.map (fun p -> p.PresetRef.name) preset_opt |> Option.get
  else
    let user_name = Upath.get_attr "UserName" "Value" xml in
    if user_name <> "" then
      user_name
    else
      Xml.get_name xml


(* ================== Device modules ================== *)
module RegularDevice = struct
  (** All the built-in devices *)
  type t = regular_device [@@deriving eq]

  let create (xml : Xml.t) : t =
    match xml with
    | Xml.Element { name; _ } ->
      let id = Alsdiff_base.Xml.get_int_attr "Id" xml in
      let pointee = Alsdiff_base.Upath.get_int_attr "/Pointee" "Id" xml in
      let preset = Upath.find_opt "/LastPresetRef/Value/*" xml |> Option.map snd |> Option.map PresetRef.create in
      let display_name = get_display_name preset xml in
      let enabled = Upath.find "/On" xml |> DeviceParam.create_from_upath_find in

      (* Find all elements that has both LomId and Manual child elements *)
      let params = Alsdiff_base.Upath.find_all "/**/LomId/../Manual/.." xml
        |> List.map DeviceParam.create_from_upath_find
      in
      { id; device_name=name; display_name; pointee; enabled; params; preset }
    | _ -> raise (Xml.Xml_error (xml, "Invalid XML element for creating Device"))

  module Patch = struct
    type t = regular_device_patch

    (* Use reference to break circular dependency - initialized after
       the mutually recursive helpers are defined at end of file *)
    let is_empty_ref : (t -> bool) ref = ref (fun _ -> failwith "RegularDevice.Patch.is_empty not initialized")
    let is_empty p = !is_empty_ref p
  end

  let diff (old_device : t) (new_device : t) : Patch.t =
    regular_device_diff old_device new_device

  module ViewSpec(B : Alsdiff_view_spec_types.View_spec_types.S) = struct
    module DP = DeviceParam.ViewSpec(B)

    let build_section_name (c : (t, Patch.t) structured_change) : string =
      match c with
      | `Added v | `Removed v ->
        Printf.sprintf "%s (#%d): %s" v.device_name v.id v.display_name
      | `Modified p ->
        let display = match p.display_name with
          | `Modified { newval; _ } -> newval
          | `Unchanged -> p.device_name
        in Printf.sprintf "%s (#%d): %s" p.device_name p.id display
      | `Unchanged -> ""

    let build_param_item (c : (DeviceParam.t, DeviceParam.Patch.t) structured_change) =
      let name = match c with
        | `Added dp -> dp.base.name
        | `Removed dp -> dp.base.name
        | `Modified p -> (match p.base with `Modified gp -> gp.name | `Unchanged -> "Parameter")
        | `Unchanged -> "Parameter"
      in DP.build_item ~name c

    let field_specs = [
      B.make_string "Display Name" (fun (d : t) -> d.display_name) (fun (p : Patch.t) -> p.display_name);
    ]
    let section_specs = [
      B.Spec.inline_fields ~specs:field_specs ~domain_type:(B.domain_type_of_name "DTDevice");
      B.Spec.collection ~name:"Parameters"
        ~of_value:(fun (d : t) -> d.params)
        ~of_patch:(fun (p : Patch.t) -> p.params)
        ~build_item:build_param_item
        ~domain_type:(B.domain_type_of_name "DTParam");
    ]
    let build_value_fields ?(domain_type = B.domain_type_of_name "DTDevice") ct v =
      B.build_value_field_views field_specs ct v ~domain_type
    let build_patch_fields ?(domain_type = B.domain_type_of_name "DTDevice") p =
      B.build_patch_field_views field_specs p ~domain_type
    let build_item ?(name = "") ?(domain_type = B.domain_type_of_name "DTDevice") c =
      B.build_item_from_specs ~name ~domain_type ~specs:section_specs c
  end
end


module PluginDevice = struct
  type t = plugin_device [@@deriving eq]

  let create (xml : Xml.t) : t =
    (* Get device ID *)
    let id = Xml.get_int_attr "Id" xml in

    let preset = Upath.find_opt "/LastPresetRef/Value/*" xml
      |> Option.map snd
      |> Option.map PresetRef.create in
    let display_name = get_display_name preset xml in

    (* Get pointee ID *)
    let pointee = Upath.get_int_attr "/Pointee" "Id" xml in
    let enabled = Upath.find "/On" xml |> DeviceParam.create_from_upath_find in

    (* Find the PluginDesc element - must be before device_name *)
    let plugin_desc_xml = Upath.find "/PluginDesc" xml |> snd in
    let desc = PluginDesc.create plugin_desc_xml in

    (* Use actual plugin name from desc instead of XML element name *)
    let device_name = desc.name in

    (* Extract all plugin parameters *)
    let params =
      Upath.find_all "/ParameterList/*" xml
      |> List.map snd
      |> List.map PluginParam.create
    in

    { id; device_name; display_name; pointee; enabled; desc; params; preset }


  module Patch = struct
    type t = plugin_device_patch

    (* Use reference to break circular dependency - initialized after
       the mutually recursive helpers are defined at end of file *)
    let is_empty_ref : (t -> bool) ref = ref (fun _ -> failwith "PluginDevice.Patch.is_empty not initialized")
    let is_empty p = !is_empty_ref p
  end

  let has_same_id (a : t) (b : t) = a.id = b.id

  let diff = plugin_device_diff

  module ViewSpec(B : Alsdiff_view_spec_types.View_spec_types.S) = struct
    module DP = DeviceParam.ViewSpec(B)
    module PP = PluginParam.ViewSpec(B)
    module PD = PluginDesc.ViewSpec(B)
    module PR = PresetRef.ViewSpec(B)

    let build_section_name (c : (t, Patch.t) structured_change) : string =
      match c with
      | `Added v | `Removed v ->
        Printf.sprintf "%s (#%d): %s" v.device_name v.id v.display_name
      | `Modified p ->
        let display = match p.display_name with
          | `Modified { newval; _ } -> newval
          | `Unchanged -> p.device_name
        in Printf.sprintf "%s (#%d): %s" p.device_name p.id display
      | `Unchanged -> ""

    let build_param_item (c : (PluginParam.t, PluginParam.Patch.t) structured_change) =
      let name = match c with
        | `Added pp -> pp.base.name
        | `Removed pp -> pp.base.name
        | `Modified p -> (match p.base with `Modified gp -> gp.name | `Unchanged -> "PluginParam")
        | `Unchanged -> "PluginParam"
      in PP.build_item ~name c

    let field_specs = [
      B.make_string "Display Name" (fun (d : t) -> d.display_name) (fun (p : Patch.t) -> p.display_name);
    ]
    let section_specs = [
      B.Spec.inline_fields ~specs:field_specs ~domain_type:(B.domain_type_of_name "DTDevice");
      B.Spec.child_optional ~name:"Preset"
        ~of_value:(fun (d : t) -> d.preset)
        ~of_patch:(fun (p : Patch.t) -> p.preset)
        ~build_value_children:(fun ct pr -> PR.build_value_fields ct pr)
        ~build_patch_children:(fun p -> PR.build_patch_fields p)
        ~domain_type:(B.domain_type_of_name "DTPreset");
      B.Spec.child ~name:"Enabled"
        ~of_value:(fun (d : t) -> d.enabled)
        ~of_patch:(fun (p : Patch.t) -> p.enabled)
        ~build_value_children:(fun ct dp -> DP.build_value_fields ct dp)
        ~build_patch_children:(fun p -> DP.build_patch_fields p)
        ~domain_type:(B.domain_type_of_name "DTParam");
      B.Spec.collection ~name:"Parameters"
        ~of_value:(fun (d : t) -> d.params)
        ~of_patch:(fun (p : Patch.t) -> p.params)
        ~build_item:build_param_item
        ~domain_type:(B.domain_type_of_name "DTParam");
      B.Spec.child ~name:"PluginDesc"
        ~of_value:(fun (d : t) -> d.desc)
        ~of_patch:(fun (p : Patch.t) -> p.desc)
        ~build_value_children:(fun ct desc -> PD.build_value_fields ct desc)
        ~build_patch_children:(fun p -> PD.build_patch_fields p)
        ~domain_type:(B.domain_type_of_name "DTDevice");
    ]
    let build_value_fields ?(domain_type = B.domain_type_of_name "DTDevice") ct v =
      B.build_value_field_views field_specs ct v ~domain_type
    let build_patch_fields ?(domain_type = B.domain_type_of_name "DTDevice") p =
      B.build_patch_field_views field_specs p ~domain_type
    let build_item ?(name = "") ?(domain_type = B.domain_type_of_name "DTDevice") c =
      B.build_item_from_specs ~name ~domain_type ~specs:section_specs c
  end
end


module Branch = struct
  type t = branch [@@deriving eq]

  let has_same_id a b = a.id = b.id

  let id_hash t = Hashtbl.hash t.id

  let create (device_creator : Xml.t -> device) (xml : Xml.t) : t =
    let id = Xml.get_int_attr "Id" xml in
    let mixer = Upath.find "MixerDevice" xml |> snd |> MixerDevice.create in
    let devices = Upath.find "/DeviceChain/*/Devices" xml
      |> snd
      |> Xml.get_childs
      |> List.map device_creator
    in
    { id; devices; mixer }

  module Patch = struct
    type t = branch_patch

    (* Use references to break circular dependency - initialized after
       the mutually recursive helpers are defined at end of file *)
    let is_empty_ref : (t -> bool) ref = ref (fun _ -> failwith "Branch.Patch.is_empty not initialized")
    let is_empty p = !is_empty_ref p
  end

  let diff = branch_diff
end


module GroupDevice = struct
  type t = group_device [@@deriving eq]

  let has_same_id a b = a.id = b.id

  let id_hash t = Hashtbl.hash t.id

  let create (device_creator : Xml.t -> device) (xml : Xml.t) : t =
    match xml with
    | Xml.Element { name; _ } ->
      let id = Xml.get_int_attr "Id" xml in
      let pointee = Upath.get_int_attr "/Pointee" "Id" xml in
      let preset = Upath.find_opt "/LastPresetRef/Value/*" xml
        |> Option.map snd
        |> Option.map PresetRef.create in
      let display_name = get_display_name preset xml in
      let enabled = Upath.find "/On" xml |> DeviceParam.create_from_upath_find in

      let branches = Upath.find "/Branches" xml
        |> snd
        |> Xml.get_childs
        |> List.map (Branch.create device_creator)
      in
      let macro_names_xml = Upath.find_all "/'MacroDisplayNames\\.[0-9]+$'" xml in
      let macro_controls_xml = Upath.find_all "/'MacroControls\\.[0-9]+$'" xml in

      (* Create ordered list of macros by index *)
      let macros =
        List.combine macro_names_xml macro_controls_xml
        |> List.map (fun (n,c) ->
            let element_name = match (snd n) with
              | Xml.Element { name; _ } -> name
              | Xml.Data _ -> raise (Xml.Xml_error (snd n, "Expected Element, got Data"))
            in
            let index = extract_index_from_name element_name in
            let macro = Macro.create (snd n) (snd c) in
            (index, macro)
          )
        |> List.sort (fun (i1, _) (i2, _) -> Stdlib.compare i1 i2)
        |> List.map snd
      in
      let snapshots =
        Upath.find_all "/MacroVariations/MacroSnapshots/MacroSnapshot" xml
        |> List.map snd
        |> List.map Snapshot.create
      in

      { id; device_name=name; display_name; pointee;  enabled; branches; macros; snapshots; preset }
    | _ -> invalid_arg "Cannot create a GroupDevice on Data"

  module Patch = struct
    type t = group_device_patch

    (* Use reference to break circular dependency - initialized after
       the mutually recursive helpers are defined at end of file *)
    let is_empty_ref : (t -> bool) ref = ref (fun _ -> failwith "GroupDevice.Patch.is_empty not initialized")
    let is_empty p = !is_empty_ref p
  end

  let diff = group_device_diff

  module ViewSpec(B : Alsdiff_view_spec_types.View_spec_types.S) = struct
    module MVS = Macro.ViewSpec(B)
    module SVS = Snapshot.ViewSpec(B)
    module PR = PresetRef.ViewSpec(B)

    let build_section_name (c : (t, Patch.t) structured_change) : string =
      match c with
      | `Added v | `Removed v ->
        Printf.sprintf "%s (#%d): %s" v.device_name v.id v.display_name
      | `Modified p ->
        let display = match p.display_name with
          | `Modified { newval; _ } -> newval
          | `Unchanged -> p.device_name
        in Printf.sprintf "%s (#%d): %s" p.device_name p.id display
      | `Unchanged -> ""

    let build_macro_item (c : (Macro.t, Macro.Patch.t) structured_change) =
      MVS.build_item ~name:"Macro" c

    let build_snapshot_item (c : (Snapshot.t, Snapshot.Patch.t) structured_change) =
      let name = match c with
        | `Added s -> s.name | `Removed s -> s.name
        | `Modified _ | `Unchanged -> "Snapshot"
      in SVS.build_item ~name c

    let field_specs = [
      B.make_string "Display Name" (fun (d : t) -> d.display_name) (fun (p : Patch.t) -> p.display_name);
    ]
    let section_specs = [
      B.Spec.inline_fields ~specs:field_specs ~domain_type:(B.domain_type_of_name "DTDevice");
      B.Spec.child_optional ~name:"Preset"
        ~of_value:(fun (d : t) -> d.preset)
        ~of_patch:(fun (p : Patch.t) -> p.preset)
        ~build_value_children:(fun ct pr -> PR.build_value_fields ct pr)
        ~build_patch_children:(fun p -> PR.build_patch_fields p)
        ~domain_type:(B.domain_type_of_name "DTPreset");
      B.Spec.collection ~name:"Macros"
        ~of_value:(fun (d : t) -> d.macros)
        ~of_patch:(fun (p : Patch.t) -> p.macros)
        ~build_item:build_macro_item
        ~domain_type:(B.domain_type_of_name "DTMacro");
      B.Spec.collection ~name:"Snapshots"
        ~of_value:(fun (d : t) -> d.snapshots)
        ~of_patch:(fun (p : Patch.t) -> p.snapshots)
        ~build_item:build_snapshot_item
        ~domain_type:(B.domain_type_of_name "DTSnapshot");
    ]
    let build_value_fields ?(domain_type = B.domain_type_of_name "DTDevice") ct v =
      B.build_value_field_views field_specs ct v ~domain_type
    let build_patch_fields ?(domain_type = B.domain_type_of_name "DTDevice") p =
      B.build_patch_field_views field_specs p ~domain_type
    let build_item ?(name = "") ?(domain_type = B.domain_type_of_name "DTDevice") c =
      B.build_item_from_specs ~name ~domain_type ~specs:section_specs c
  end
end


module Max4LiveDevice = struct
  type t = max4live_device [@@deriving eq]

  let create (xml : Xml.t) : t =
    (* Get device ID *)
    let id = Xml.get_int_attr "Id" xml in

    let preset = Upath.find_opt "/LastPresetRef/Value/*" xml
      |> Option.map snd
      |> Option.map PresetRef.create in
    let display_name = get_display_name preset xml in

    (* Get pointee ID *)
    let pointee = Upath.get_int_attr "/Pointee" "Id" xml in
    let enabled = Upath.find "/On" xml |> DeviceParam.create_from_upath_find in

    (* Parse PatchSlot/MxPatchRef for patch_ref - must be before device_name *)
    let patch_ref = Upath.find "/PatchSlot/Value/'MxD?PatchRef'" xml |> snd |> PatchRef.create in

    (* Use patch name instead of XML element name *)
    let device_name = patch_ref.PatchRef.name in

    (* Extract all M4L parameters *)
    let float_params = Alsdiff_base.Upath.find_all "**/MxDFloatParameter" xml in
    let int_params = Alsdiff_base.Upath.find_all "**/MxDIntParameter" xml in
    let bool_params = Alsdiff_base.Upath.find_all "**/MxDBoolParameter" xml in
    let enum_params = Alsdiff_base.Upath.find_all "**/MxDEnumParameter" xml in
    let all_params = float_params @ int_params @ bool_params @ enum_params in
    let params = List.map Max4LiveParam.create_from_upath_find all_params in

    { id; device_name; display_name; pointee; enabled; patch_ref; params; preset }

  module Patch = struct
    type t = max4live_device_patch

    (* Use reference to break circular dependency - initialized after
       the mutually recursive helpers are defined at end of file *)
    let is_empty_ref : (t -> bool) ref = ref (fun _ -> failwith "Max4LiveDevice.Patch.is_empty not initialized")
    let is_empty p = !is_empty_ref p
  end

  let has_same_id (a : t) (b : t) = a.id = b.id

  let diff = max4live_device_diff

  module ViewSpec(B : Alsdiff_view_spec_types.View_spec_types.S) = struct
    module M4LP = Max4LiveParam.ViewSpec(B)
    module PR = PresetRef.ViewSpec(B)
    module PatchR = PatchRef.ViewSpec(B)

    let build_section_name (c : (t, Patch.t) structured_change) : string =
      match c with
      | `Added v | `Removed v ->
        Printf.sprintf "%s (#%d): %s" v.device_name v.id v.display_name
      | `Modified p ->
        let display = match p.display_name with
          | `Modified { newval; _ } -> newval
          | `Unchanged -> p.device_name
        in Printf.sprintf "%s (#%d): %s" p.device_name p.id display
      | `Unchanged -> ""

    let build_param_item (c : (Max4LiveParam.t, Max4LiveParam.Patch.t) structured_change) =
      let name = match c with
        | `Added mp -> mp.base.name
        | `Removed mp -> mp.base.name
        | `Modified p -> (match p.base with `Modified gp -> gp.name | `Unchanged -> "M4LParam")
        | `Unchanged -> "M4LParam"
      in M4LP.build_item ~name c

    let field_specs = [
      B.make_string "Display Name" (fun (d : t) -> d.display_name) (fun (p : Patch.t) -> p.display_name);
    ]
    let section_specs = [
      B.Spec.inline_fields ~specs:field_specs ~domain_type:(B.domain_type_of_name "DTDevice");
      B.Spec.child_optional ~name:"Preset"
        ~of_value:(fun (d : t) -> d.preset)
        ~of_patch:(fun (p : Patch.t) -> p.preset)
        ~build_value_children:(fun ct pr -> PR.build_value_fields ct pr)
        ~build_patch_children:(fun p -> PR.build_patch_fields p)
        ~domain_type:(B.domain_type_of_name "DTPreset");
      B.Spec.child ~name:"PatchRef"
        ~of_value:(fun (d : t) -> d.patch_ref)
        ~of_patch:(fun (p : Patch.t) ->
            match p.patch_ref with `Modified pr -> `Modified pr | _ -> `Unchanged)
        ~build_value_children:(fun ct pr -> PatchR.build_value_fields ct pr)
        ~build_patch_children:(fun p -> PatchR.build_patch_fields p)
        ~domain_type:(B.domain_type_of_name "DTPreset");
      B.Spec.collection ~name:"Parameters"
        ~of_value:(fun (d : t) -> d.params)
        ~of_patch:(fun (p : Patch.t) -> p.params)
        ~build_item:build_param_item
        ~domain_type:(B.domain_type_of_name "DTParam");
    ]
    let build_value_fields ?(domain_type = B.domain_type_of_name "DTDevice") ct v =
      B.build_value_field_views field_specs ct v ~domain_type
    let build_patch_fields ?(domain_type = B.domain_type_of_name "DTDevice") p =
      B.build_patch_field_views field_specs p ~domain_type
    let build_item ?(name = "") ?(domain_type = B.domain_type_of_name "DTDevice") c =
      B.build_item_from_specs ~name ~domain_type ~specs:section_specs c
  end
end


type t = device [@@deriving eq]

let rec create (xml : Xml.t) : t =
  match xml with
  | Xml.Element { name; _ } ->
    (match name with
     | "InstrumentGroupDevice" | "DrumGroupDevice" | "MidiEffectGroupDevice" | "AudioEffectGroupDevice" ->
       Group (GroupDevice.create create xml)
     | "PluginDevice" | "AuPluginDevice" ->
       Plugin (PluginDevice.create xml)
     | "MxDeviceInstrument" | "MxDeviceAudioEffect" | "MxDeviceMidiEffect" ->
       Max4Live (Max4LiveDevice.create xml)
     | _ -> Regular (RegularDevice.create xml))

  | _ -> invalid_arg "Cannot create a Device on Data"

let has_same_id old_device new_device =
  match old_device, new_device with
  | Regular old_reg, Regular new_reg -> old_reg.id = new_reg.id
  | Plugin old_plug, Plugin new_plug -> old_plug.id = new_plug.id
  | Group old_group, Group new_group -> old_group.id = new_group.id
  | Max4Live old_m4l, Max4Live new_m4l -> old_m4l.id = new_m4l.id
  | _ -> false

let id_hash device =
  match device with
  | Regular reg -> Hashtbl.hash reg.id
  | Plugin plug -> Hashtbl.hash plug.id
  | Max4Live m4l -> Hashtbl.hash m4l.id
  | Group group -> Hashtbl.hash group.id

module Patch = struct
  type t =
    | RegularPatch of RegularDevice.Patch.t
    | PluginPatch of PluginDevice.Patch.t
    | Max4LivePatch of Max4LiveDevice.Patch.t
    | GroupPatch of GroupDevice.Patch.t

  (* Use reference to break circular dependency - initialized after
     the mutually recursive helpers are defined *)
  let is_empty_ref : (t -> bool) ref = ref (fun _ -> failwith "is_empty not initialized")
  let is_empty p = !is_empty_ref p
end

(* ================== Mutually Recursive is_empty Helpers ================== *)
(* These functions are defined here at the end of the file where all modules
   are available, to properly handle the circular dependency between
   Branch.Patch and Device.Patch. *)

let rec is_unchanged_branch_change = function
  | `Added _ | `Removed _ -> false
  | `Unchanged -> true
  | `Modified p -> branch_patch_is_empty p

and is_unchanged_device_change = function
  | `Added _ | `Removed _ -> false
  | `Unchanged -> true
  | `Modified p -> device_patch_is_empty p

and branch_patch_is_empty (p : branch_patch) =
  is_unchanged_atomic_update p.id &&
  is_unchanged_update (module MixerDevice.Patch) p.mixer &&
  List.for_all is_unchanged_device_change p.devices

and device_patch_is_empty = function
  | RegularPatch rp ->
    is_unchanged_atomic_update rp.display_name &&
    is_unchanged_change (module PresetRef.Patch) rp.preset &&
    List.for_all (is_unchanged_change (module DeviceParam.Patch)) rp.params
  | PluginPatch pp ->
    is_unchanged_atomic_update pp.display_name &&
    is_unchanged_update (module DeviceParam.Patch) pp.enabled &&
    is_unchanged_update (module PluginDesc.Patch) pp.desc &&
    is_unchanged_change (module PresetRef.Patch) pp.preset &&
    List.for_all (is_unchanged_change (module PluginParam.Patch)) pp.params
  | Max4LivePatch mp ->
    is_unchanged_atomic_update mp.display_name &&
    is_unchanged_update (module DeviceParam.Patch) mp.enabled &&
    is_unchanged_change (module PatchRef.Patch) mp.patch_ref &&
    is_unchanged_change (module PresetRef.Patch) mp.preset &&
    List.for_all (is_unchanged_change (module Max4LiveParam.Patch)) mp.params
  | GroupPatch gp ->
    is_unchanged_atomic_update gp.display_name &&
    is_unchanged_update (module DeviceParam.Patch) gp.enabled &&
    is_unchanged_change (module PresetRef.Patch) gp.preset &&
    List.for_all is_unchanged_branch_change gp.branches &&
    List.for_all (is_unchanged_change (module Macro.Patch)) gp.macros &&
    List.for_all (is_unchanged_change (module Snapshot.Patch)) gp.snapshots

(* ================== Initialize Forward References ================== *)
(* Initialize all mutable references for is_empty functions.
   This must come after the mutually recursive helpers are defined. *)

(* Note: Patch.t and device_patch are nominally different types in OCaml,
   so we need this wrapper to convert between them *)
let () = Patch.is_empty_ref := (function
    | Patch.RegularPatch p -> device_patch_is_empty (RegularPatch p)
    | Patch.PluginPatch p -> device_patch_is_empty (PluginPatch p)
    | Patch.Max4LivePatch p -> device_patch_is_empty (Max4LivePatch p)
    | Patch.GroupPatch p -> device_patch_is_empty (GroupPatch p))

let () = device_patch_is_empty_ref := device_patch_is_empty
let () = branch_patch_is_empty_ref := branch_patch_is_empty
let () = Branch.Patch.is_empty_ref := branch_patch_is_empty

(* Initialize submodule is_empty references using centralized logic *)
let () = RegularDevice.Patch.is_empty_ref := (fun p -> device_patch_is_empty (RegularPatch p))
let () = PluginDevice.Patch.is_empty_ref := (fun p -> device_patch_is_empty (PluginPatch p))
let () = Max4LiveDevice.Patch.is_empty_ref := (fun p -> device_patch_is_empty (Max4LivePatch p))
let () = GroupDevice.Patch.is_empty_ref := (fun p -> device_patch_is_empty (GroupPatch p))



let diff (old_device : t) (new_device : t) : Patch.t =
  match (old_device, new_device) with
  | (Regular old_reg, Regular new_reg) ->
    let patch = RegularDevice.diff old_reg new_reg in
    Patch.RegularPatch patch
  | (Group old_group, Group new_group) ->
    let patch = GroupDevice.diff old_group new_group in
    Patch.GroupPatch patch
  | (Plugin old_plug, Plugin new_plug) ->
    let patch = PluginDevice.diff old_plug new_plug in
    Patch.PluginPatch patch
  | (Max4Live old_m4l, Max4Live new_m4l) ->
    let patch = Max4LiveDevice.diff old_m4l new_m4l in
    Patch.Max4LivePatch patch
  | _ ->
    let t1, id1 = get_device_type_and_id old_device in
    let t2, id2 = get_device_type_and_id new_device in
    failwith (Printf.sprintf "Cannot diff devices of different types: %s(Id=%d) vs %s(Id=%d)"
                t1 id1 t2 id2)
