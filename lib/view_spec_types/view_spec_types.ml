[@@@warning "-unused-var"]
open Alsdiff_base.Diff

type domain_type
type change_type
type field_value =
  | Fint of int
  | Ffloat of float
  | Fbool of bool
  | Fstring of string

and view
and item
and collection

and dual_time_formatter = {
  format_old : float -> field_value;
  format_new : float -> field_value;
}

type ('v, 'p) unified_field_spec = {
  name : string;
  get_value : 'v -> field_value;
  get_old_value : 'v -> field_value option;
  get_patch : 'p -> field_value atomic_update;
}

type ('v, 'p) section_spec = {
  name : string;
  build : ('v, 'p) structured_change -> view option;
}

let int_value x = Fint x
let float_value x = Ffloat x
let bool_value x = Fbool x
let string_value x = Fstring x

let default_domain_type : domain_type = Obj.magic ()

let domain_type_of_name (_ : string) : domain_type = Obj.magic ()

let format_unix_timestamp (_ : int) : string = ""

let make_spec
    (wrapper : 'a -> field_value)
    (name : string)
    (get_v : 'v -> 'a)
    (get_p : 'p -> 'a atomic_update)
  : ('v, 'p) unified_field_spec =
  { name; get_value = (fun v -> wrapper (get_v v)); get_old_value = (fun _ -> None);
    get_patch = (fun p -> match get_p p with
        | `Unchanged -> `Unchanged
        | `Modified { oldval; newval } ->
          `Modified { oldval = wrapper oldval; newval = wrapper newval }) }

let make_spec_const
    (wrapper : 'a -> field_value)
    (name : string)
    (get_v : 'v -> 'a)
  : ('v, 'p) unified_field_spec =
  { name; get_value = (fun v -> wrapper (get_v v)); get_old_value = (fun _ -> None);
    get_patch = (fun _ -> `Unchanged) }

let make_int n v p = make_spec int_value n v p
let make_float n v p = make_spec float_value n v p
let make_string n v p = make_spec string_value n v p
let make_bool n v p = make_spec bool_value n v p

let make_time_field (fmt : dual_time_formatter) name get_v get_p = {
  name;
  get_value = (fun v -> fmt.format_new (get_v v));
  get_old_value = (fun v -> Some (fmt.format_old (get_v v)));
  get_patch = (fun p -> match get_p p with
      | `Unchanged -> `Unchanged
      | `Modified { oldval; newval } ->
        `Modified { oldval = fmt.format_old oldval; newval = fmt.format_new newval });
}

let build_value_field_views
    (_specs : ('v, 'p) unified_field_spec list)
    (_change_type : change_type)
    (_value : 'v)
    ~(_domain_type : domain_type)
  : view list = Obj.magic ()

let build_patch_field_views
    (_specs : ('v, 'p) unified_field_spec list)
    (_patch : 'p)
    ~(_domain_type : domain_type)
  : view list = Obj.magic ()

let remap_spec :
  ('v2 -> 'v1) -> ('p2 -> 'p1 structured_update) ->
  ('v1, 'p1) unified_field_spec -> ('v2, 'p2) unified_field_spec =
  fun f_v f_p spec ->
  { (spec) with
    get_value = (fun v -> spec.get_value (f_v v));
    get_old_value = (fun v -> spec.get_old_value (f_v v));
    get_patch = (fun p ->
        match f_p p with
        | `Modified bp -> spec.get_patch bp
        | `Unchanged -> `Unchanged) }

let map_specs
    (f_v : 'v2 -> 'v1)
    (f_p : 'p2 -> 'p1 structured_update)
    (specs : ('v1, 'p1) unified_field_spec list)
  : ('v2, 'p2) unified_field_spec list =
  List.map (remap_spec f_v f_p) specs

let build_item_from_specs
    ~(_name : string) ~(_domain_type : domain_type) ~(_specs : ('v, 'p) section_spec list)
    (_ : ('v, 'p) structured_change) : item = Obj.magic ()

module Spec = struct
  let inline_fields
      (type v p)
      ~(_specs : (v, p) unified_field_spec list)
      ~(_domain_type : domain_type)
    : (v, p) section_spec = Obj.magic ()

  let child
      (type parent patch nested np)
      ~(_name : string)
      ~(_of_value : parent -> nested)
      ~(_of_patch : patch -> np structured_update)
      ~(_build_value_children : change_type -> nested -> view list)
      ~(_build_patch_children : np -> view list)
      ~(_domain_type : domain_type)
    : (parent, patch) section_spec = Obj.magic ()

  let child_optional
      (type parent patch nested np)
      ~(_name : string)
      ~(_of_value : parent -> nested option)
      ~(_of_patch : patch -> (nested, np) structured_change)
      ~(_build_value_children : change_type -> nested -> view list)
      ~(_build_patch_children : np -> view list)
      ~(_domain_type : domain_type)
    : (parent, patch) section_spec = Obj.magic ()

  let collection
      (type parent patch elem ep)
      ~(_name : string)
      ~(_of_value : parent -> elem list)
      ~(_of_patch : patch -> (elem, ep) structured_change list)
      ~(_build_item : (elem, ep) structured_change -> item)
      ~(_domain_type : domain_type)
    : (parent, patch) section_spec = Obj.magic ()
end

module type S = sig
  type domain_type
  type change_type
  type field_value =
    | Fint of int
    | Ffloat of float
    | Fbool of bool
    | Fstring of string

  and view
  and item
  and collection

  and dual_time_formatter = {
    format_old : float -> field_value;
    format_new : float -> field_value;
  }

  type ('v, 'p) unified_field_spec
  type ('v, 'p) section_spec

  val int_value : int -> field_value
  val float_value : float -> field_value
  val bool_value : bool -> field_value
  val string_value : string -> field_value
  val default_domain_type : domain_type
  val domain_type_of_name : string -> domain_type
  val format_unix_timestamp : int -> string

  val make_spec :
    ('a -> field_value) -> string -> ('v -> 'a) -> ('p -> 'a atomic_update) ->
    ('v, 'p) unified_field_spec
  val make_spec_const :
    ('a -> field_value) -> string -> ('v -> 'a) ->
    ('v, 'p) unified_field_spec
  val make_int : string -> ('v -> int) -> ('p -> int atomic_update) -> ('v, 'p) unified_field_spec
  val make_float : string -> ('v -> float) -> ('p -> float atomic_update) -> ('v, 'p) unified_field_spec
  val make_string : string -> ('v -> string) -> ('p -> string atomic_update) -> ('v, 'p) unified_field_spec
  val make_bool : string -> ('v -> bool) -> ('p -> bool atomic_update) -> ('v, 'p) unified_field_spec
  val make_time_field :
    dual_time_formatter -> string -> ('v -> float) -> ('p -> float atomic_update) ->
    ('v, 'p) unified_field_spec

  val build_value_field_views :
    ('v, 'p) unified_field_spec list -> change_type -> 'v -> domain_type:domain_type -> view list
  val build_patch_field_views :
    ('v, 'p) unified_field_spec list -> 'p -> domain_type:domain_type -> view list

  val map_specs :
    ('v2 -> 'v1) ->
    ('p2 -> 'p1 structured_update) ->
    ('v1, 'p1) unified_field_spec list ->
    ('v2, 'p2) unified_field_spec list

  val build_item_from_specs :
    name:string -> domain_type:domain_type -> specs:('v, 'p) section_spec list ->
    ('v, 'p) structured_change -> item

  module Spec : sig
    val inline_fields :
      specs:('v, 'p) unified_field_spec list -> domain_type:domain_type -> ('v, 'p) section_spec
    val child :
      name:string ->
      of_value:('parent -> 'nested) ->
      of_patch:('patch -> 'np structured_update) ->
      build_value_children:(change_type -> 'nested -> view list) ->
      build_patch_children:('np -> view list) ->
      domain_type:domain_type ->
      ('parent, 'patch) section_spec
    val child_optional :
      name:string ->
      of_value:('parent -> 'nested option) ->
      of_patch:('patch -> ('nested, 'np) structured_change) ->
      build_value_children:(change_type -> 'nested -> view list) ->
      build_patch_children:('np -> view list) ->
      domain_type:domain_type ->
      ('parent, 'patch) section_spec
    val collection :
      name:string ->
      of_value:('parent -> 'elem list) ->
      of_patch:('patch -> ('elem, 'ep) structured_change list) ->
      build_item:(('elem, 'ep) structured_change -> item) ->
      domain_type:domain_type ->
      ('parent, 'patch) section_spec
  end
end
