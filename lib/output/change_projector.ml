open Alsdiff_live
open Alsdiff_base.Diff
open Output_types
open Display_context
open Presentation_model

(** [option_to_list] converts an option to a list. *)
let option_to_list = function
  | Some x -> [x]
  | None -> []


(** ViewBuilder module - uses the unified 3-type system (Field, Item, Collection) *)
module ViewBuilder = struct

  (** [change_type_of c] extracts the change type from a structured change. *)
  let change_type_of (c : ('a, 'p) structured_change) : change_type =
    match c with
    | `Added _ -> Added
    | `Removed _ -> Removed
    | `Modified _ -> Modified
    | `Unchanged -> Unchanged

  let map_atomic_change (f : 'a -> 'b) (c : 'a atomic_change) : 'b atomic_change =
    match c with
    | `Added x -> `Added (f x)
    | `Removed x -> `Removed (f x)
    | `Modified { oldval; newval } -> `Modified { oldval = f oldval; newval = f newval }
    | `Unchanged -> `Unchanged

  let map_atomic_update (f : 'a -> 'b) (u : 'a atomic_update) : 'b atomic_update =
    match u with
    | `Modified { oldval; newval } -> `Modified { oldval = f oldval; newval = f newval }
    | `Unchanged -> `Unchanged

  let map_atomic_update_dual (f_old : 'a -> 'b) (f_new : 'a -> 'b) (u : 'a atomic_update) : 'b atomic_update =
    match u with
    | `Modified { oldval; newval } -> `Modified { oldval = f_old oldval; newval = f_new newval }
    | `Unchanged -> `Unchanged


  (** [build_item_from_children c ~name ~of_value ~of_patch ~build_value_children ~build_patch_children]
      builds a [item] with arbitrary children for a nested structured type.

      This is the new equivalent of [build_nested_section_view], but returns a [item] instead
      of [section_view], and can contain any [new_view] children (not just [view]).

      @param c the parent structured change
      @param name the item name
      @param of_value extracts the nested value from the parent value
      @param of_patch extracts the nested update from the parent patch
      @param build_value_children builds view list from nested value and change type
      @param build_patch_children builds view list from nested patch
      @param domain_type the domain type for this item
      @return Some item if there are children, None otherwise
  *)
  let build_item_from_children
      (c : ('parent, 'pp) structured_change)
      ~(name : string)
      ~(of_value : 'parent -> 'nested)
      ~(of_patch : 'pp -> 'np structured_update)
      ~(build_value_children : change_type -> 'nested -> view list)
      ~(build_patch_children : 'np -> view list)
      ~(domain_type : domain_type)
    : item option =
    match c with
    | `Added parent ->
      let nested_val = of_value parent in
      let children = build_value_children Added nested_val in
      if children = [] then None
      else Some { name; change = Added; domain_type; children }
    | `Removed parent ->
      let nested_val = of_value parent in
      let children = build_value_children Removed nested_val in
      if children = [] then None
      else Some { name; change = Removed; domain_type; children }
    | `Modified patch ->
      (match of_patch patch with
       | `Unchanged ->
         (* Nested content is unchanged but parent is Modified.
            Create a placeholder item with empty children - the rendering layer
            will decide whether to show it based on the preset. *)
         Some { name; change = Unchanged; domain_type; children = [] }
       | `Modified np ->
         let children = build_patch_children np in
         if children = [] then None
         else Some { name; change = Modified; domain_type; children })
    | `Unchanged ->
      (* Parent is unchanged - we don't have access to the value to extract nested content.
         This is a fundamental limitation - unchanged items don't carry their values. *)
      None


  (** [build_item_from_children_with_change c ~name ~of_value ~of_patch ~build_value_children ~build_patch_children]
      builds a [item] for a nested structured type that may be added/removed independently.

      This is the new equivalent of [build_nested_section_view_with_change], but returns a [item].

      @param c the parent structured change
      @param name the item name
      @param of_value extracts the nested value from the parent value
      @param of_patch extracts the nested change from the parent patch
      @param build_value_children builds view list from nested value and change type
      @param build_patch_children builds view list from nested patch
      @param domain_type the domain type for this item
      @return Some item if there are children, None otherwise
  *)
  let build_item_from_children_with_change
      (type parent pp nested_actual np)
      (c : (parent, pp) structured_change)
      ~(name : string)
      ~(of_value : parent -> nested_actual option)
      ~(of_patch : pp -> (nested_actual, np) structured_change)
      ~(build_value_children : change_type -> nested_actual -> view list)
      ~(build_patch_children : np -> view list)
      ~(domain_type : domain_type)
    : item option =
    match c with
    | `Added parent ->
      (match of_value parent with
       | None -> None
       | Some nested_val ->
         let children = build_value_children Added nested_val in
         if children = [] then None
         else Some { name; change = Added; domain_type; children })
    | `Removed parent ->
      (match of_value parent with
       | None -> None
       | Some nested_val ->
         let children = build_value_children Removed nested_val in
         if children = [] then None
         else Some { name; change = Removed; domain_type; children })
    | `Modified patch ->
      (match of_patch patch with
       | `Unchanged -> None
       | `Added nested_val ->
         let children = build_value_children Added nested_val in
         if children = [] then None
         else Some { name; change = Modified; domain_type; children }
       | `Removed nested_val ->
         let children = build_value_children Removed nested_val in
         if children = [] then None
         else Some { name; change = Modified; domain_type; children }
       | `Modified np ->
         let children = build_patch_children np in
         if children = [] then None
         else Some { name; change = Modified; domain_type; children })
    | `Unchanged -> None


  (** [build_collection c ~name ~of_value ~of_patch ~build_item]
      builds a [new_collection] for a list field containing structured items.

      This is the new equivalent of [build_collection_view], but:
      - Returns [new_collection] instead of [collection_view]
      - The [build_item] function should return a [item] (which gets wrapped in [Item])
      - Items in the collection can have full structure (not simplified [element_view])

      @param c the parent structured change
      @param name the collection name
      @param of_value extracts the item list from the parent value
      @param of_patch extracts the change list from the parent patch
      @param build_item builds a item from an item change
      @param domain_type the domain type for this collection
      @return Some new_collection if there are items, None otherwise
  *)
  let build_collection
      (c : ('parent, 'pp) structured_change)
      ~(name : string)
      ~(of_value : 'parent -> 'item list)
      ~(of_patch : 'pp -> ('item, 'ip) structured_change list)
      ~(build_item : ('item, 'ip) structured_change -> item)
      ~(domain_type : domain_type)
    : collection option =
    let change_type = change_type_of c in
    let items = match c with
      | `Added parent ->
        parent |> of_value |> List.map (fun item -> Item (build_item (`Added item)))
      | `Removed parent ->
        parent |> of_value |> List.map (fun item -> Item (build_item (`Removed item)))
      | `Modified patch ->
        patch |> of_patch |> List.map (fun item_change -> Item (build_item item_change))
      | `Unchanged -> []
    in
    (* Filter out Unchanged items and placeholder items (for unchanged items where we don't have values) *)
    let items = List.filter (fun (i : view) ->
        match i with
        | Item item -> item.change <> Unchanged && item.name <> ""
        | Collection col -> col.change <> Unchanged
        | Field _ -> true
      ) items in
    if items = [] then None
    else Some { name; change = change_type; domain_type; items }

end


(** [structured_update_to_field_views] flattens a [structured_update] into multiple [field_view] items.
    @param build_fields function that takes the patch and returns a list of field options
    @param update the structured update
*)
let structured_update_to_field_views
    ~(build_fields : 'p -> field option list)
    (update : 'p structured_update)
  : field list =
  match update with
  | `Unchanged -> []
  | `Modified patch ->
    patch |> build_fields |> List.filter_map Fun.id



(* ==================== Unified Field Spec System ==================== *)

(** A unified field specification that can generate field views for both
    Added/Removed (from value) and Modified (from patch) cases.
    This eliminates the need for paired create_X_fields / create_X_patch_fields functions.
*)
type ('value, 'patch) unified_field_spec = {
  name : string;
  get_value : 'value -> field_value;                          (** Extract field value from parent *)
  get_old_value : 'value -> field_value option;               (** None = use get_value *)
  get_patch : 'patch -> field_value atomic_update;            (** Extract field update from patch *)
}


(** [build_value_field_views specs change_type value] builds field views from a value.
    Used for Added/Removed cases.
    @param specs the list of unified field specs
    @param change_type the type of change (Added or Removed)
    @param value the parent value
    @param domain_type the domain type for these fields
*)
let build_value_field_views
    (specs : ('v, 'p) unified_field_spec list)
    (change_type : change_type)
    (value : 'v)
    ~(domain_type : domain_type)
  : view list =
  specs |> List.map (fun spec ->
      let old_val = match spec.get_old_value value with
        | Some fv -> fv
        | None -> spec.get_value value
      in
      (Field {
          name = spec.name;
          change = change_type;
          domain_type;
          oldval = (if change_type = Removed then Some old_val else None);
          newval = (if change_type = Added then Some (spec.get_value value) else None);
        } : view))


(** [build_patch_field_views specs patch] builds field views from a patch.
    Used for Modified cases. Only returns fields that have actually changed.
    @param specs the list of unified field specs
    @param patch the parent patch
    @param domain_type the domain type for these fields
*)
let build_patch_field_views
    (specs : ('v, 'p) unified_field_spec list)
    (patch : 'p)
    ~(domain_type : domain_type)
  : view list =
  specs
  |> List.filter_map (fun spec ->
      let update = spec.get_patch patch in
      match update with
      | `Unchanged -> None
      | `Modified { oldval; newval } ->
        Some ((Field {
            name = spec.name;
            change = Modified;
            domain_type;
            oldval = Some oldval;
            newval = Some newval;
          } : view))
    )

let map_specs
    (f_v : 'v2 -> 'v1)
    (f_p : 'p2 -> 'p1 structured_update)
    (specs : ('v1, 'p1) unified_field_spec list)
  : ('v2, 'p2) unified_field_spec list =
  List.map (fun spec ->
      { (spec) with
        get_value = (fun v -> spec.get_value (f_v v));
        get_old_value = (fun v -> spec.get_old_value (f_v v));
        get_patch = (fun p ->
            match f_p p with
            | `Modified bp -> spec.get_patch bp
            | `Unchanged -> `Unchanged)
      }) specs


(** A specification for building a child section of an Item *)
type ('parent, 'patch) section_spec = {
  name : string;
  build : ('parent, 'patch) structured_change -> view option;
}


(** Spec module - combinators for building section_spec values declaratively *)
module Spec = struct

  (** [inline_fields ~specs ~domain_type] builds inline field views from unified field specs.
      Returns Field views wrapped in an Item with empty name (""), automatically filtering
      out Unchanged fields. The empty name signals to [build_item_from_specs] that this
      Item's children should be inlined directly into the parent.
  *)
  let inline_fields
      (type v p)
      ~(specs : (v, p) unified_field_spec list)
      ~(domain_type : domain_type)
    : (v, p) section_spec =
    {
      name = "";
      build = (fun c ->
          let fields = match c with
            | `Added value ->
              build_value_field_views specs Added value ~domain_type
            | `Removed value ->
              build_value_field_views specs Removed value ~domain_type
            | `Modified patch ->
              build_patch_field_views specs patch ~domain_type
            | `Unchanged -> []
          in
          (* Filter out Unchanged fields *)
          let filtered = List.filter (function
              | Field f -> f.change <> Unchanged
              | _ -> true
            ) fields in
          if filtered = [] then None
          else Some (Item { name = ""; change = ViewBuilder.change_type_of c; domain_type; children = filtered })
        );
    }

  (** [child ~name ~of_value ~of_patch ~build_value_children ~build_patch_children ~domain_type]
      builds a section_spec for a nested item (e.g., Mixer, Loop).
  *)
  let child
      (type parent patch nested np)
      ~(name : string)
      ~(of_value : parent -> nested)
      ~(of_patch : patch -> np structured_update)
      ~(build_value_children : change_type -> nested -> view list)
      ~(build_patch_children : np -> view list)
      ~(domain_type : domain_type)
    : (parent, patch) section_spec =
    {
      name;
      build = (fun c ->
          ViewBuilder.build_item_from_children c
            ~name
            ~of_value
            ~of_patch
            ~build_value_children
            ~build_patch_children
            ~domain_type
          |> Option.map (fun i -> Item i)
        );
    }

  (** [child_optional ~name ~of_value ~of_patch ~build_value_children ~build_patch_children ~domain_type]
      builds a section_spec for a nested item that can be added/removed independently.
  *)
  let child_optional
      (type parent patch nested np)
      ~(name : string)
      ~(of_value : parent -> nested option)
      ~(of_patch : patch -> (nested, np) structured_change)
      ~(build_value_children : change_type -> nested -> view list)
      ~(build_patch_children : np -> view list)
      ~(domain_type : domain_type)
    : (parent, patch) section_spec =
    {
      name;
      build = (fun c ->
          ViewBuilder.build_item_from_children_with_change c
            ~name
            ~of_value
            ~of_patch
            ~build_value_children
            ~build_patch_children
            ~domain_type
          |> Option.map (fun i -> Item i)
        );
    }

  (** [collection ~name ~of_value ~of_patch ~build_item ~domain_type]
      builds a section_spec for a collection of items.
  *)
  let collection
      (type parent patch elem ep)
      ~(name : string)
      ~(of_value : parent -> elem list)
      ~(of_patch : patch -> (elem, ep) structured_change list)
      ~(build_item : (elem, ep) structured_change -> item)
      ~(domain_type : domain_type)
    : (parent, patch) section_spec =
    {
      name;
      build = (fun c ->
          ViewBuilder.build_collection c
            ~name
            ~of_value
            ~of_patch
            ~build_item
            ~domain_type
          |> Option.map (fun col -> Collection col)
        );
    }

end


(** [build_item_from_specs ~name ~domain_type ~specs c] builds an item from a list of section specs.
    This is the main entry point for declaratively building complex items.
    Each spec in the list is applied to the change, and the resulting views are concatenated.

    For inline_fields specs (name = ""), the children are extracted and added directly.
    For other specs, the resulting view is added as-is.
*)
let build_item_from_specs
    (type parent patch)
    ~(name : string)
    ~(domain_type : domain_type)
    ~(specs : (parent, patch) section_spec list)
    (c : (parent, patch) structured_change)
  : item =
  let change_type = ViewBuilder.change_type_of c in
  let children = specs |> List.filter_map (fun spec ->
      match spec.build c with
      | None -> None
      | Some view ->
        (* For inline_fields (name = ""), extract children directly *)
        if spec.name = "" then
          match view with
          | Item { children; _ } -> Some children
          | _ -> Some [view]
        else
          Some [view]
    ) |> List.flatten in
  { name; change = change_type; domain_type; children }


(** [child_from_specs ~name ~of_value ~of_patch ~specs ~child_domain_type ~domain_type]
    builds a section_spec for a nested item using its own section_spec list.
    This eliminates the need for manual paired build_value/build_patch functions.
*)
let child_from_specs
    (type parent patch nested np)
    ~(name : string)
    ~(of_value : parent -> nested)
    ~(of_patch : patch -> np structured_update)
    ~(specs : (nested, np) section_spec list)
    ~(child_domain_type : domain_type)
    ~(domain_type : domain_type)
  : (parent, patch) section_spec =
  let build_value ct v =
    let c : (nested, np) structured_change =
      match ct with Added -> `Added v | Removed -> `Removed v | _ -> `Unchanged
    in
    let item = build_item_from_specs ~name ~domain_type:child_domain_type ~specs c in
    item.children
  and build_patch p =
    let item = build_item_from_specs ~name ~domain_type:child_domain_type ~specs (`Modified p) in
    item.children
  in
  Spec.child ~name ~of_value ~of_patch
    ~build_value_children:build_value
    ~build_patch_children:build_patch
    ~domain_type


(** Helper functions for creating field descriptors with common wrappers *)
let make_spec
    (wrapper : 'a -> field_value)
    (name : string)
    (get_v : 'v -> 'a)
    (get_p : 'p -> 'a atomic_update)
  : ('v, 'p) unified_field_spec =
  {
    name;
    get_value = (fun v -> wrapper (get_v v));
    get_old_value = (fun _ -> None);
    get_patch = (fun p -> ViewBuilder.map_atomic_update wrapper (get_p p));
  }

(** [make_spec_const wrapper name get_v] creates a unified field spec for a value that never changes in a patch (e.g. name). *)
let make_spec_const
    (wrapper : 'a -> field_value)
    (name : string)
    (get_v : 'v -> 'a)
  : ('v, 'p) unified_field_spec =
  {
    name;
    get_value = (fun v -> wrapper (get_v v));
    get_old_value = (fun _ -> None);
    get_patch = (fun _ -> `Unchanged);
  }

let make_int n v p = make_spec int_value n v p
let make_float n v p = make_spec float_value n v p
let make_string n v p = make_spec string_value n v p
let make_bool n v p = make_spec bool_value n v p

let make_time_field (fmt : dual_time_formatter) name get_v get_p = {
  name;
  get_value = (fun v -> fmt.format_new (get_v v));
  get_old_value = (fun v -> Some (fmt.format_old (get_v v)));
  get_patch = (fun p -> ViewBuilder.map_atomic_update_dual fmt.format_old fmt.format_new (get_p p));
}


(** Loop field specifications *)
let loop_field_specs ?(format_time = default_dual_time_formatter) () : (Clip.Loop.t, Clip.Loop.Patch.t) unified_field_spec list = [
  make_time_field format_time "Start Time" (fun (l : Clip.Loop.t) -> l.start_time) (fun (p : Clip.Loop.Patch.t) -> p.start_time);
  make_time_field format_time "End Time" (fun (l : Clip.Loop.t) -> l.end_time) (fun (p : Clip.Loop.Patch.t) -> p.end_time);
  make_bool "On" (fun (l : Clip.Loop.t) -> l.on) (fun (p : Clip.Loop.Patch.t) -> p.on);
]

let create_loop_fields ?(format_time = default_dual_time_formatter) =
  build_value_field_views (loop_field_specs ~format_time ()) ~domain_type:DTLoop
let create_loop_patch_fields ?(format_time = default_dual_time_formatter) =
  build_patch_field_views (loop_field_specs ~format_time ()) ~domain_type:DTLoop



(** CurveControls field specifications *)
let curve_controls_field_specs : (Automation.CurveControls.t, Automation.CurveControls.Patch.t) unified_field_spec list = [
  make_float "Curve1 X" (fun (c : Automation.CurveControls.t) -> c.curve1_x) (fun (p : Automation.CurveControls.Patch.t) -> p.curve1_x);
  make_float "Curve1 Y" (fun (c : Automation.CurveControls.t) -> c.curve1_y) (fun (p : Automation.CurveControls.Patch.t) -> p.curve1_y);
  make_float "Curve2 X" (fun (c : Automation.CurveControls.t) -> c.curve2_x) (fun (p : Automation.CurveControls.Patch.t) -> p.curve2_x);
  make_float "Curve2 Y" (fun (c : Automation.CurveControls.t) -> c.curve2_y) (fun (p : Automation.CurveControls.Patch.t) -> p.curve2_y);
]

let create_curve_controls_fields = build_value_field_views curve_controls_field_specs ~domain_type:DTEvent
let create_curve_controls_patch_fields = build_patch_field_views curve_controls_field_specs ~domain_type:DTEvent

(* Default note name style for MIDI notes *)
let default_note_name_style = Sharp

(** [create_note_item] builds a [item] for a single note change (new type system).
    @param note_name_style the style to use for note names (Sharp or Flat)
    @param c the note structured change
*)
let create_note_item
    ?(note_name_style : note_display_style = default_note_name_style)
    ?(format_time : dual_time_formatter = default_dual_time_formatter)
    (c : (Clip.MidiNote.t, Clip.MidiNote.Patch.t) structured_change)
  : item =
  let open Clip.MidiNote in
  let specs = [
    make_time_field format_time "Time" (fun (x : t) -> x.time) (fun (x : Patch.t) -> x.time);
    make_float "Duration" (fun (x : t) -> x.duration) (fun (x : Patch.t) -> x.duration);
    make_float "Velocity" (fun (x : t) -> x.velocity) (fun (x : Patch.t) -> x.velocity);
    make_int "Note" (fun (x : t) -> x.note) (fun (x : Patch.t) -> x.note);
    make_float "Off Velocity" (fun (x : t) -> x.off_velocity) (fun (x : Patch.t) -> x.off_velocity);
  ]
  in
  let note_name = match c with
    | `Added n ->
      let name = get_note_name_from_int ~style:note_name_style n.note in
      Printf.sprintf "Note %s (%d)" name n.note
    | `Removed n ->
      let name = get_note_name_from_int ~style:note_name_style n.note in
      Printf.sprintf "Note %s (%d)" name n.note
    | `Modified _ -> "Note"
    | `Unchanged -> "Note"
  in
  let section_spec = Spec.inline_fields ~specs ~domain_type:DTNote in
  build_item_from_specs ~name:note_name ~domain_type:DTNote ~specs:[section_spec] c



(** [event_value_to_field_value] converts an Automation.event_value to a field_value *)
let event_value_to_field_value v =
  match v with
  | Automation.FloatEvent f -> Ffloat f
  | Automation.IntEvent i -> Fint i
  | Automation.EnumEvent e -> Fint e

(** [event_value_atomic_to_field_value] converts an event_value atomic_update to field_value atomic_update *)
let event_value_atomic_to_field_value (update : Automation.event_value atomic_update) : field_value atomic_update =
  match update with
  | `Modified { oldval; newval } ->
    `Modified { oldval = event_value_to_field_value oldval; newval = event_value_to_field_value newval }
  | `Unchanged -> `Unchanged


(** [create_events_item] builds a [item] for an envelope event change (new type system).
    @param c the envelope event structured change
*)
let create_events_item
    ?(format_time : dual_time_formatter = default_dual_time_formatter)
    (c : (Automation.EnvelopeEvent.t, Automation.EnvelopeEvent.Patch.t) structured_change)
  : item =
  let open Automation in
  let base_specs = [
    make_time_field format_time "Time" (fun (x : EnvelopeEvent.t) -> x.time) (fun (x : EnvelopeEvent.Patch.t) -> x.time);
    make_spec event_value_to_field_value "Value"
      (fun (x : EnvelopeEvent.t) -> x.value) (fun (x : EnvelopeEvent.Patch.t) -> x.value);
  ]
  in
  let curve_section_spec = Spec.child_optional
      ~name:"Curve"
      ~of_value:(fun (e : EnvelopeEvent.t) -> e.curve)
      ~of_patch:(fun (p : EnvelopeEvent.Patch.t) -> p.curve)
      ~build_value_children:create_curve_controls_fields
      ~build_patch_children:create_curve_controls_patch_fields
      ~domain_type:DTEvent
  in
  let base_section_spec = Spec.inline_fields ~specs:base_specs ~domain_type:DTEvent in
  build_item_from_specs ~name:"EnvelopeEvent" ~domain_type:DTEvent ~specs:[base_section_spec; curve_section_spec] c


(* ================== Device View Functions ==================== *)


(* ==================== MidiClip Specs (using PPX + manual Loop) ==================== *)

(** [build_clip_section_name ~clip_type ~get_id ~get_name ~get_patch_id ~get_patch_name c]
    builds a section name for any clip type.
    @param clip_type The clip type label (e.g., "MidiClip", "AudioClip")
    @param get_id Extracts the ID from a clip value
    @param get_name Extracts the name from a clip value
    @param get_patch_id Extracts the ID from a clip patch
    @param get_patch_name Extracts the name atomic update from a clip patch
*)
let build_clip_section_name
    (type v p)
    ~(clip_type : string)
    ~(get_id : v -> int)
    ~(get_name : v -> string)
    ~(get_patch_id : p -> int)
    ~(get_patch_name : p -> string atomic_update)
    (c : (v, p) structured_change)
  : string =
  match c with
  | `Added clip -> Printf.sprintf "%s (#%d): %s" clip_type (get_id clip) (get_name clip)
  | `Removed clip -> Printf.sprintf "%s (#%d): %s" clip_type (get_id clip) (get_name clip)
  | `Modified patch ->
    (match get_patch_name patch with
     | `Modified { newval; _ } -> Printf.sprintf "%s (#%d): %s" clip_type (get_patch_id patch) newval
     | `Unchanged -> Printf.sprintf "%s (#%d)" clip_type (get_patch_id patch))
  | `Unchanged -> clip_type

(** [build_midi_clip_section_name] builds the section name for a MidiClip. *)
let build_midi_clip_section_name =
  build_clip_section_name
    ~clip_type:"MidiClip"
    ~get_id:(fun c -> c.Clip.MidiClip.id)
    ~get_name:(fun c -> c.Clip.MidiClip.name)
    ~get_patch_id:(fun p -> p.Clip.MidiClip.Patch.id)
    ~get_patch_name:(fun p -> p.Clip.MidiClip.Patch.name)

(* ==================== AudioClip name helper ==================== *)

(** [build_audio_clip_section_name] builds the section name for an AudioClip. *)
let build_audio_clip_section_name =
  build_clip_section_name
    ~clip_type:"AudioClip"
    ~get_id:(fun c -> c.Clip.AudioClip.id)
    ~get_name:(fun c -> c.Clip.AudioClip.name)
    ~get_patch_id:(fun p -> p.Clip.AudioClip.Patch.id)
    ~get_patch_name:(fun p -> p.Clip.AudioClip.Patch.name)




(* ==================== Device ViewSpec Instantiations ==================== *)

module DeviceViewSpecB : Alsdiff_view_spec_types.View_spec_types.S
  with type domain_type = Output_types.domain_type
   and type change_type = Output_types.change_type
   and type field_value = Output_types.field_value
   and type view = Presentation_model.view
   and type item = Presentation_model.item
   and type collection = Presentation_model.collection
   and type dual_time_formatter = Display_context.dual_time_formatter
   and type ('v, 'p) section_spec = ('v, 'p) section_spec
   and type ('v, 'p) unified_field_spec = ('v, 'p) unified_field_spec
= struct
  type domain_type = Output_types.domain_type
  type change_type = Output_types.change_type
  type field_value = Output_types.field_value =
    | Fint of int
    | Ffloat of float
    | Fbool of bool
    | Fstring of string
  and view = Presentation_model.view =
    | Field of field
    | Item of item
    | Collection of collection
  and item = Presentation_model.item = {
    name : string;
    change : change_type;
    domain_type : domain_type;
    children : view list;
  }
  and collection = Presentation_model.collection = {
    name : string;
    change : change_type;
    domain_type : domain_type;
    items : view list;
  }
  and dual_time_formatter = Display_context.dual_time_formatter = {
    format_old : float -> field_value;
    format_new : float -> field_value;
  }

  type nonrec ('v, 'p) unified_field_spec = ('v, 'p) unified_field_spec
  type nonrec ('v, 'p) section_spec = ('v, 'p) section_spec

  let int_value = int_value
  let float_value = float_value
  let bool_value = bool_value
  let string_value = string_value
  let default_domain_type = DTOther
  let domain_type_of_name = function
    | "DTLiveset" -> DTLiveset
    | "DTTrack" -> DTTrack
    | "DTDevice" -> DTDevice
    | "DTClip" -> DTClip
    | "DTAutomation" -> DTAutomation
    | "DTMixer" -> DTMixer
    | "DTRouting" -> DTRouting
    | "DTLocator" -> DTLocator
    | "DTParam" -> DTParam
    | "DTNote" -> DTNote
    | "DTEvent" -> DTEvent
    | "DTSend" -> DTSend
    | "DTPreset" -> DTPreset
    | "DTMacro" -> DTMacro
    | "DTSnapshot" -> DTSnapshot
    | "DTLoop" -> DTLoop
    | "DTSignature" -> DTSignature
    | "DTSampleRef" -> DTSampleRef
    | "DTVersion" -> DTVersion
    | _ -> DTOther
  let format_unix_timestamp = Display_context.format_unix_timestamp

  let make_spec = make_spec
  let make_spec_const = make_spec_const
  let make_int = make_int
  let make_float = make_float
  let make_string = make_string
  let make_bool = make_bool
  let make_time_field = make_time_field

  let build_value_field_views = build_value_field_views
  let build_patch_field_views = build_patch_field_views
  let map_specs = map_specs
  let build_item_from_specs = build_item_from_specs

  module Spec = Spec
end

module RegularDeviceVS = Device.RegularDevice.ViewSpec(DeviceViewSpecB)
module PluginDeviceVS = Device.PluginDevice.ViewSpec(DeviceViewSpecB)
module Max4LiveDeviceVS = Device.Max4LiveDevice.ViewSpec(DeviceViewSpecB)
module GroupDeviceVS = Device.GroupDevice.ViewSpec(DeviceViewSpecB)
module MidiTrackVS = Track.MidiTrack.ViewSpec(DeviceViewSpecB)
module AudioTrackVS = Track.AudioTrack.ViewSpec(DeviceViewSpecB)
module MainTrackVS = Track.MainTrack.ViewSpec(DeviceViewSpecB)
module MixerVS = Track.Mixer.ViewSpec(DeviceViewSpecB)
module MainMixerVS = Track.MainMixer.ViewSpec(DeviceViewSpecB)
module RoutingSetVS = Track.RoutingSet.ViewSpec(DeviceViewSpecB)
module MidiClipVS = Clip.MidiClip.ViewSpec(DeviceViewSpecB)
module AudioClipVS = Clip.AudioClip.ViewSpec(DeviceViewSpecB)


(* ==================== Clip Item Builders (after VS instantiations) ==================== *)

(** [create_midi_clip_item] creates a [item] from a MidiClip structured change.
    PPX generates inline fields (name, start/end time), TimeSignature child, and Notes collection.
    Loop is manually composed because the PPX's child spec generator doesn't thread format_time to children.
*)
let create_midi_clip_item
    ?(note_name_style : note_display_style = default_note_name_style)
    ?(format_time : dual_time_formatter = default_dual_time_formatter)
    (c : (Clip.MidiClip.t, Clip.MidiClip.Patch.t) structured_change)
  : item =
  let name = build_midi_clip_section_name c in
  let loop_child = Spec.child ~name:"Loop"
      ~of_value:(fun (c : Clip.MidiClip.t) -> c.loop)
      ~of_patch:(fun (p : Clip.MidiClip.Patch.t) -> p.loop)
      ~build_value_children:(create_loop_fields ~format_time)
      ~build_patch_children:(create_loop_patch_fields ~format_time)
      ~domain_type:DTLoop in
  let ppx_specs = MidiClipVS.section_specs ~format_time
      ~build_notes:(create_note_item ~note_name_style ~format_time) in
  let specs = match ppx_specs with
    | first :: rest -> first :: loop_child :: rest
    | _ -> assert false
  in
  build_item_from_specs ~name ~domain_type:DTClip ~specs c

(** [create_audio_clip_item] creates a [item] from an AudioClip structured change.
    PPX generates inline fields (name, start/end time), TimeSignature child, SampleRef child, and Fade child.
    Loop is manually composed because the PPX's child spec generator doesn't thread format_time to children.
*)
let create_audio_clip_item
    ?(format_time : dual_time_formatter = default_dual_time_formatter)
    (c : (Clip.AudioClip.t, Clip.AudioClip.Patch.t) structured_change)
  : item =
  let name = build_audio_clip_section_name c in
  let loop_child = Spec.child ~name:"Loop"
      ~of_value:(fun (c : Clip.AudioClip.t) -> c.loop)
      ~of_patch:(fun (p : Clip.AudioClip.Patch.t) -> p.loop)
      ~build_value_children:(create_loop_fields ~format_time)
      ~build_patch_children:(create_loop_patch_fields ~format_time)
      ~domain_type:DTLoop in
  let ppx_specs = AudioClipVS.section_specs ~format_time in
  let specs = match ppx_specs with
    | first :: rest -> first :: loop_child :: rest
    | _ -> assert false
  in
  build_item_from_specs ~name ~domain_type:DTClip ~specs c


(* ==================== Track Element Views ==================== *)


(** [create_automation_item] builds a [item] for an automation change (new type system).
    @param get_pointee_name function to resolve pointee IDs to names
    @param c the automation structured change
*)
let create_automation_item
    ~(get_pointee_name : int -> string)
    ?(format_time : dual_time_formatter = default_dual_time_formatter)
    (c : (Automation.t, Automation.Patch.t) structured_change)
  : item =
  let open Automation in
  let change_type = ViewBuilder.change_type_of c in
  let automation_name = match c with
    | `Added a -> Printf.sprintf "Automation (id=%d, target=%s)" a.id (get_pointee_name a.target)
    | `Removed r -> Printf.sprintf "Automation (id=%d, target=%s)" r.id (get_pointee_name r.target)
    | `Modified patch -> Printf.sprintf "Automation (id=%d, target=%s)" patch.id (get_pointee_name patch.target)
    | `Unchanged -> "Automation"
  in

  let event_children : view list = match c with
    | `Modified patch ->
      patch.events |> List.mapi (fun i event_change ->
          let event_id = match event_change with
            | `Added e -> e.Automation.EnvelopeEvent.id
            | `Removed e -> e.Automation.EnvelopeEvent.id
            | `Modified _ -> i
            | `Unchanged -> -1
          in
          match event_change with
          | `Unchanged -> None
          | _ ->
            let event_item = create_events_item ~format_time event_change in
            Some (Item { event_item with name = Printf.sprintf "Event[%d]" event_id })
        ) |> List.filter_map Fun.id
    | `Added _ | `Removed _ | `Unchanged -> []
  in

  { name = automation_name; change = change_type; domain_type = DTAutomation; children = event_children }


(** [create_device_item] builds a [item] for a device change (new type system).
    @param c the device structured change
*)
let create_device_item
    (c : (Device.t, Device.Patch.t) structured_change)
  : item =
  match c with
  | `Added (Device.Regular d) ->
    RegularDeviceVS.build_item ~name:(RegularDeviceVS.build_section_name (`Added d))
      ~domain_type:DTDevice (`Added d)
  | `Removed (Device.Regular d) ->
    RegularDeviceVS.build_item ~name:(RegularDeviceVS.build_section_name (`Removed d))
      ~domain_type:DTDevice (`Removed d)
  | `Modified (Device.Patch.RegularPatch p) ->
    RegularDeviceVS.build_item ~name:(RegularDeviceVS.build_section_name (`Modified p))
      ~domain_type:DTDevice (`Modified p)
  | `Added (Device.Plugin d) ->
    PluginDeviceVS.build_item ~name:(PluginDeviceVS.build_section_name (`Added d))
      ~domain_type:DTDevice (`Added d)
  | `Removed (Device.Plugin d) ->
    PluginDeviceVS.build_item ~name:(PluginDeviceVS.build_section_name (`Removed d))
      ~domain_type:DTDevice (`Removed d)
  | `Modified (Device.Patch.PluginPatch p) ->
    PluginDeviceVS.build_item ~name:(PluginDeviceVS.build_section_name (`Modified p))
      ~domain_type:DTDevice (`Modified p)
  | `Added (Device.Max4Live d) ->
    Max4LiveDeviceVS.build_item ~name:(Max4LiveDeviceVS.build_section_name (`Added d))
      ~domain_type:DTDevice (`Added d)
  | `Removed (Device.Max4Live d) ->
    Max4LiveDeviceVS.build_item ~name:(Max4LiveDeviceVS.build_section_name (`Removed d))
      ~domain_type:DTDevice (`Removed d)
  | `Modified (Device.Patch.Max4LivePatch p) ->
    Max4LiveDeviceVS.build_item ~name:(Max4LiveDeviceVS.build_section_name (`Modified p))
      ~domain_type:DTDevice (`Modified p)
  | `Added (Device.Group d) ->
    GroupDeviceVS.build_item ~name:(GroupDeviceVS.build_section_name (`Added d))
      ~domain_type:DTDevice (`Added d)
  | `Removed (Device.Group d) ->
    GroupDeviceVS.build_item ~name:(GroupDeviceVS.build_section_name (`Removed d))
      ~domain_type:DTDevice (`Removed d)
  | `Modified (Device.Patch.GroupPatch p) ->
    GroupDeviceVS.build_item ~name:(GroupDeviceVS.build_section_name (`Modified p))
      ~domain_type:DTDevice (`Modified p)
  | `Unchanged ->
    { name = "Device"; change = Unchanged; domain_type = DTDevice; children = [] }


(* ==================== Full Track Views ==================== *)


(** [create_midi_track_item] creates a [item] from a MidiTrack structured change (new type system).
    @param get_pointee_name function to resolve pointee IDs to names
    @param note_name_style the style to use for note names (Sharp or Flat)
    @param c the MIDI track structured change
*)
let create_midi_track_item
    ~(get_pointee_name : int -> string)
    ?(note_name_style : note_display_style = default_note_name_style)
    ?(format_time : dual_time_formatter = default_dual_time_formatter)
    (c : (Track.MidiTrack.t, Track.MidiTrack.Patch.t) structured_change)
  : item =
  MidiTrackVS.build_item
    ~build_clips:(create_midi_clip_item ~note_name_style ~format_time)
    ~build_automations:(create_automation_item ~get_pointee_name ~format_time)
    ~build_devices:create_device_item
    ~name:(MidiTrackVS.build_section_name c)
    ~domain_type:DTTrack c

(** [create_audio_like_track_item] creates a [item] for AudioTrack-like structured changes.
    Shared implementation for AudioTrack and GroupTrack (which share the same internal structure).
    @param get_pointee_name function to resolve pointee IDs to names
    @param track_type_name The display type name (e.g., "AudioTrack" or "Group")
    @param c the track structured change
*)
let create_audio_like_track_item
    ~(get_pointee_name : int -> string)
    ?(format_time : dual_time_formatter = default_dual_time_formatter)
    ~track_type_name
    (c : (Track.AudioTrack.t, Track.AudioTrack.Patch.t) structured_change)
  : item =
  AudioTrackVS.build_item
    ~build_clips:(create_audio_clip_item ~format_time)
    ~build_automations:(create_automation_item ~get_pointee_name ~format_time)
    ~build_devices:create_device_item
    ~name:(AudioTrackVS.build_section_name ~type_label:track_type_name c)
    ~domain_type:DTTrack c


let create_audio_track_item
    ~(get_pointee_name : int -> string)
    ?(note_name_style : note_display_style = default_note_name_style)
    ?(format_time : dual_time_formatter = default_dual_time_formatter)
    (c : (Track.AudioTrack.t, Track.AudioTrack.Patch.t) structured_change)
  : item =
  ignore (note_name_style : note_display_style);
  create_audio_like_track_item ~get_pointee_name ~format_time ~track_type_name:"AudioTrack" c


let create_group_track_item
    ~(get_pointee_name : int -> string)
    ?(note_name_style : note_display_style = default_note_name_style)
    ?(format_time : dual_time_formatter = default_dual_time_formatter)
    (c : (Track.AudioTrack.t, Track.AudioTrack.Patch.t) structured_change)
  : item =
  ignore (note_name_style : note_display_style);
  create_audio_like_track_item ~get_pointee_name ~format_time ~track_type_name:"Group" c


(** [create_main_track_item] creates a [item] from a MainTrack structured change (new type system).
    @param get_pointee_name function to resolve pointee IDs to names
    @param c the main track structured change
*)
let create_main_track_item
    ~(get_pointee_name : int -> string)
    ?(note_name_style : note_display_style = default_note_name_style)
    ?(format_time : dual_time_formatter = default_dual_time_formatter)
    (c : (Track.MainTrack.t, Track.MainTrack.Patch.t) structured_change)
  : item =
  ignore (note_name_style : note_display_style);
  MainTrackVS.build_item
    ~build_automations:(create_automation_item ~get_pointee_name ~format_time)
    ~build_devices:create_device_item
    ~name:(MainTrackVS.build_section_name c)
    ~domain_type:DTTrack c


(* ==================== Liveset View ==================== *)

let locator_field_specs ?(format_time = default_dual_time_formatter) () : (Liveset.Locator.t, Liveset.Locator.Patch.t) unified_field_spec list = [
  make_spec_const int_value "Id" (fun (x : Liveset.Locator.t) -> x.id);
  make_string "Name" (fun (x : Liveset.Locator.t) -> x.name) (fun (p : Liveset.Locator.Patch.t) -> p.name);
  make_time_field format_time "Time" (fun (x : Liveset.Locator.t) -> x.time) (fun (p : Liveset.Locator.Patch.t) -> p.time);
]

let locator_section_specs ?(format_time = default_dual_time_formatter) () : (Liveset.Locator.t, Liveset.Locator.Patch.t) section_spec list = [
  Spec.inline_fields ~specs:(locator_field_specs ~format_time ()) ~domain_type:DTLocator;
]

let create_locator_item
    ?(format_time : dual_time_formatter = default_dual_time_formatter)
    (c : (Liveset.Locator.t, Liveset.Locator.Patch.t) structured_change)
  : item =
  let locator_name = match c with
    | `Added l -> Printf.sprintf "Locator (id=%d)" l.Liveset.Locator.id
    | `Removed l -> Printf.sprintf "Locator (id=%d)" l.Liveset.Locator.id
    | `Modified _ -> "Locator"
    | `Unchanged -> "Locator"
  in
  build_item_from_specs ~name:locator_name ~domain_type:DTLocator ~specs:(locator_section_specs ~format_time ()) c


(** Version field specifications *)
let version_field_specs : (Liveset.Version.t, Liveset.Version.Patch.t) unified_field_spec list = [
  { name = "Major";
    get_value = (fun v -> string_value v.major);
    get_old_value = (fun _ -> None);
    get_patch = (fun p -> ViewBuilder.map_atomic_update string_value p.major) };
  { name = "Minor";
    get_value = (fun v -> string_value v.minor);
    get_old_value = (fun _ -> None);
    get_patch = (fun p -> ViewBuilder.map_atomic_update string_value p.minor) };
  { name = "Revision";
    get_value = (fun v -> string_value v.revision);
    get_old_value = (fun _ -> None);
    get_patch = (fun p -> ViewBuilder.map_atomic_update string_value p.revision) };
]

let create_version_fields = build_value_field_views version_field_specs ~domain_type:DTVersion
let create_version_patch_fields = build_patch_field_views version_field_specs ~domain_type:DTVersion


(* ==================== Liveset Helper Functions ==================== *)

(** [make_format_time] creates a time formatting closure based on the chosen format.
    Uses tempo and time signature events from the MainTrack for conversion.
    QuarterNotes returns Ffloat (no change), BeatTime/RealTime return Fstring.
    Precomputes sorted segments once to avoid redundant sorting per call. *)
let make_format_time (time_format : time_format)
    ~(tempo_events : (float * float * Automation.CurveControls.t option) list)
    ~(ts_events : (float * Clip.TimeSignature.t) list)
    () : float -> field_value =
  match time_format with
  | QuarterNotes -> float_value
  | BeatTime ->
    let segments = Track.MainTrack.prepare_ts_segments ts_events in
    fun x -> Fstring (format_position (Track.MainTrack.time_to_position_precomputed segments x))
  | RealTime ->
    let segments = Track.MainTrack.prepare_tempo_segments tempo_events in
    fun x -> Fstring (format_realtime (Track.MainTrack.time_to_realtime_precomputed x segments))

let make_dual_format_time (time_format : time_format)
    ~(tempo_events_old : (float * float * Automation.CurveControls.t option) list)
    ~(ts_events_old : (float * Clip.TimeSignature.t) list)
    ~(tempo_events_new : (float * float * Automation.CurveControls.t option) list)
    ~(ts_events_new : (float * Clip.TimeSignature.t) list)
    () : dual_time_formatter =
  {
    format_old = make_format_time time_format ~tempo_events:tempo_events_old ~ts_events:ts_events_old ();
    format_new = make_format_time time_format ~tempo_events:tempo_events_new ~ts_events:ts_events_new ();
  }

(** [make_pointee_resolver c] creates a pointee name resolver function from a liveset change.
    This is used to resolve automation target IDs to human-readable names.
*)
let make_pointee_resolver
    (c : (Liveset.t, Liveset.Patch.t) structured_change)
  : int -> string =
  match c with
  | `Added ls -> (fun id -> Liveset.get_pointee_name_from_table ls.Liveset.pointees id)
  | `Removed ls -> (fun id -> Liveset.get_pointee_name_from_table ls.Liveset.pointees id)
  | `Modified patch ->
    (fun id ->
       match Liveset.get_pointee_name_from_table_opt patch.Liveset.Patch.new_pointees id with
       | Some name -> name
       | None ->
         match Liveset.get_pointee_name_from_table_opt patch.Liveset.Patch.old_pointees id with
         | Some name -> name
         | None -> Printf.sprintf "<Pointee %d>" id)
  | `Unchanged -> fun id -> Printf.sprintf "<Pointee %d>" id


(** [dispatch_track_change ~get_pointee_name ~note_name_style tc] dispatches a track change to the appropriate
    track item builder based on track type.
    Returns None for Unchanged or Main tracks (Main tracks are handled separately).
*)
let dispatch_track_change
    ~(get_pointee_name : int -> string)
    ?(note_name_style : note_display_style = default_note_name_style)
    ?(format_time : dual_time_formatter = default_dual_time_formatter)
    (tc : (Track.t, Track.Patch.t) structured_change)
  : view option =
  match tc with
  (* Midi tracks *)
  | `Added (Track.Midi t) -> Some (Item (create_midi_track_item ~get_pointee_name ~note_name_style ~format_time (`Added t)))
  | `Removed (Track.Midi t) -> Some (Item (create_midi_track_item ~get_pointee_name ~note_name_style ~format_time (`Removed t)))
  | `Modified (Track.Patch.MidiPatch pt) -> Some (Item (create_midi_track_item ~get_pointee_name ~note_name_style ~format_time (`Modified pt)))
  (* Audio tracks *)
  | `Added (Track.Audio t) -> Some (Item (create_audio_track_item ~get_pointee_name ~note_name_style ~format_time (`Added t)))
  | `Removed (Track.Audio t) -> Some (Item (create_audio_track_item ~get_pointee_name ~note_name_style ~format_time (`Removed t)))
  | `Modified (Track.Patch.AudioPatch pt) -> Some (Item (create_audio_track_item ~get_pointee_name ~note_name_style ~format_time (`Modified pt)))
  (* Group tracks *)
  | `Added (Track.Group t) -> Some (Item (create_group_track_item ~get_pointee_name ~note_name_style ~format_time (`Added t)))
  | `Removed (Track.Group t) -> Some (Item (create_group_track_item ~get_pointee_name ~note_name_style ~format_time (`Removed t)))
  (* Return tracks - use audio track builder since ReturnTrack = AudioTrack *)
  | `Added (Track.Return t) -> Some (Item (create_audio_track_item ~get_pointee_name ~note_name_style ~format_time (`Added t)))
  | `Removed (Track.Return t) -> Some (Item (create_audio_track_item ~get_pointee_name ~note_name_style ~format_time (`Removed t)))
  (* Main tracks - handled separately in create_liveset_item *)
  | `Added (Track.Main _) | `Removed (Track.Main _) | `Modified (Track.Patch.MainPatch _) -> None
  | `Unchanged -> None


(** [build_liveset_tracks_items ~get_pointee_name ~note_name_style c] builds view items for all regular tracks
    (Midi, Audio, Group) in a liveset change. Main and Return tracks are handled separately.
*)
let build_liveset_tracks_items
    ~(get_pointee_name : int -> string)
    ?(note_name_style : note_display_style = default_note_name_style)
    ?(format_time : dual_time_formatter = default_dual_time_formatter)
    (c : (Liveset.t, Liveset.Patch.t) structured_change)
  : view list =
  let is_regular_track = function
    | Track.Main _ | Track.Return _ -> false
    | _ -> true
  in
  let is_regular_track_change = function
    | `Added (Track.Main _) | `Removed (Track.Main _) | `Modified (Track.Patch.MainPatch _) -> false
    | `Added (Track.Return _) | `Removed (Track.Return _) -> false
    | _ -> true
  in
  let track_changes = match c with
    | `Added ls ->
      ls.Liveset.tracks
      |> List.filter is_regular_track
      |> List.map (fun t -> `Added t)
    | `Removed ls ->
      ls.Liveset.tracks
      |> List.filter is_regular_track
      |> List.map (fun t -> `Removed t)
    | `Modified patch ->
      patch.tracks |> List.filter is_regular_track_change
    | `Unchanged -> []
  in
  List.filter_map (dispatch_track_change ~get_pointee_name ~note_name_style ~format_time) track_changes


(** [build_liveset_returns_items ~get_pointee_name ~note_name_style c] builds view items for all return tracks
    in a liveset change.
*)
let build_liveset_returns_items
    ~(get_pointee_name : int -> string)
    ?(note_name_style : note_display_style = default_note_name_style)
    ?(format_time : dual_time_formatter = default_dual_time_formatter)
    (c : (Liveset.t, Liveset.Patch.t) structured_change)
  : view list =
  let return_changes = match c with
    | `Added ls ->
      ls.Liveset.returns |> List.map (fun t -> `Added t)
    | `Removed ls ->
      ls.Liveset.returns |> List.map (fun t -> `Removed t)
    | `Modified patch -> patch.returns
    | `Unchanged -> []
  in
  List.filter_map (dispatch_track_change ~get_pointee_name ~note_name_style ~format_time) return_changes


(** Liveset field specifications for atomic fields (Name, Creator) *)
let liveset_field_specs : (Liveset.t, Liveset.Patch.t) unified_field_spec list = [
  { name = "Name";
    get_value = (fun ls -> string_value ls.Liveset.name);
    get_old_value = (fun _ -> None);
    get_patch = (fun p -> ViewBuilder.map_atomic_update string_value p.Liveset.Patch.name) };
  { name = "Creator";
    get_value = (fun ls -> string_value ls.Liveset.creator);
    get_old_value = (fun _ -> None);
    get_patch = (fun p -> ViewBuilder.map_atomic_update string_value p.Liveset.Patch.creator) };
]


(** [create_liveset_item] creates a [item] from a Liveset structured change (new type system).
    @param note_name_style the style to use for note names (Sharp or Flat)
    @param c the liveset structured change
*)
let create_liveset_item
    ?(note_name_style : note_display_style = default_note_name_style)
    ?(format_time : dual_time_formatter = default_dual_time_formatter)
    (c : (Liveset.t, Liveset.Patch.t) structured_change)
  : item =

  let change_type = ViewBuilder.change_type_of c in
  let get_pointee_name = make_pointee_resolver c in

  (* Build section name from liveset name *)
  let section_name = match c with
    | `Added ls -> "LiveSet: " ^ ls.name
    | `Removed ls -> "LiveSet: " ^ ls.name
    | `Modified patch ->
      (match patch.name with
       | `Modified { newval; _ } -> "LiveSet: " ^ newval
       | `Unchanged -> "LiveSet")
    | `Unchanged -> "LiveSet"
  in

  (* Build atomic fields using liveset_field_specs *)
  let atomic_children =
    (match c with
     | `Added v -> build_value_field_views liveset_field_specs Added v ~domain_type:DTLiveset
     | `Removed v -> build_value_field_views liveset_field_specs Removed v ~domain_type:DTLiveset
     | `Modified p -> build_patch_field_views liveset_field_specs p ~domain_type:DTLiveset
     | `Unchanged -> [])
    |> List.filter (function Field fv -> fv.change <> Unchanged | _ -> true)
  in

  (* Build Version section *)
  let version_item = ViewBuilder.build_item_from_children c
      ~name:"Version"
      ~of_value:(fun (ls : Liveset.t) -> ls.version)
      ~of_patch:(fun (p : Liveset.Patch.t) -> p.version)
      ~build_value_children:create_version_fields
      ~build_patch_children:create_version_patch_fields
      ~domain_type:DTVersion
  in

  (* Build Main Track section - special handling for singleton track *)
  let main_track_item = ViewBuilder.build_item_from_children c
      ~name:"Main Track"
      ~of_value:(fun (ls : Liveset.t) ->
          match ls.Liveset.main with
          | Track.Main t -> t
          | _ -> failwith "Liveset.main must always contain Track.Main")
      ~of_patch:(fun (p : Liveset.Patch.t) -> p.main)
      ~build_value_children:(fun ct (main_track : Track.MainTrack.t) ->
          [Item (create_main_track_item ~get_pointee_name ~note_name_style ~format_time (match ct with
               | Added -> `Added main_track
               | Removed -> `Removed main_track
               | Unchanged -> failwith "Invalid change type for value"
               | Modified -> failwith "Invalid change type for value"))])
      ~build_patch_children:(fun pt ->
          [Item (create_main_track_item ~get_pointee_name ~note_name_style ~format_time (`Modified pt))])
      ~domain_type:DTTrack
  in

  (* Build Locators collection *)
  let locators_collection = ViewBuilder.build_collection c
      ~name:"Locators"
      ~of_value:(fun (ls : Liveset.t) -> ls.Liveset.locators)
      ~of_patch:(fun (p : Liveset.Patch.t) -> p.locators)
      ~build_item:(create_locator_item ~format_time)
      ~domain_type:DTLocator
  in

  (* Combine all children *)
  let children =
    atomic_children
    @ (version_item |> Option.map (fun i -> Item i) |> option_to_list)
    @ (main_track_item |> Option.map (fun i -> Item i) |> option_to_list)
    @ build_liveset_tracks_items ~get_pointee_name ~note_name_style ~format_time c
    @ build_liveset_returns_items ~get_pointee_name ~note_name_style ~format_time c
    @ (locators_collection |> Option.map (fun c -> Collection c) |> option_to_list)
  in

  { name = section_name; change = change_type; domain_type = DTLiveset; children }


module Builder = struct
  type domain_type = Output_types.domain_type
  type change_type = Output_types.change_type
  type field_value = Output_types.field_value =
    | Fint of int
    | Ffloat of float
    | Fbool of bool
    | Fstring of string
  and view = Presentation_model.view =
    | Field of field
    | Item of item
    | Collection of collection
  and item = Presentation_model.item = {
    name : string;
    change : change_type;
    domain_type : domain_type;
    children : view list;
  }
  and collection = Presentation_model.collection = {
    name : string;
    change : change_type;
    domain_type : domain_type;
    items : view list;
  }
  and dual_time_formatter = Display_context.dual_time_formatter = {
    format_old : float -> field_value;
    format_new : float -> field_value;
  }

  let int_value = int_value
  let float_value = float_value
  let bool_value = bool_value
  let string_value = string_value
  let default_domain_type = DTOther

  let domain_type_of_name = function
    | "DTLiveset" -> DTLiveset
    | "DTTrack" -> DTTrack
    | "DTDevice" -> DTDevice
    | "DTClip" -> DTClip
    | "DTAutomation" -> DTAutomation
    | "DTMixer" -> DTMixer
    | "DTRouting" -> DTRouting
    | "DTLocator" -> DTLocator
    | "DTParam" -> DTParam
    | "DTNote" -> DTNote
    | "DTEvent" -> DTEvent
    | "DTSend" -> DTSend
    | "DTPreset" -> DTPreset
    | "DTMacro" -> DTMacro
    | "DTSnapshot" -> DTSnapshot
    | "DTLoop" -> DTLoop
    | "DTSignature" -> DTSignature
    | "DTSampleRef" -> DTSampleRef
    | "DTVersion" -> DTVersion
    | _ -> DTOther
  let format_unix_timestamp = Display_context.format_unix_timestamp
end
