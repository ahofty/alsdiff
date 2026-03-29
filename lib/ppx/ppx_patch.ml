[@@@warning "-unused-var"]
[@@@warning "-unused-open"]
open Ppxlib
module List = ListLabels

(** attribute for each field that using [patch] deriver

    - [patch.skip] skipped omitting in the [Patch.t]
*)
type patch_attribute = Skip | Id | Eq of string

(** Metadata about how to generate the is_unchanged check for a field *)
type check_kind =
  | AtomicUpdate                               (* is_unchanged_atomic_update *)
  | AtomicChange                               (* is_unchanged_atomic_change *)
  | AtomicChangeList                           (* List.for_all is_unchanged_atomic_change *)
  | AtomicVariant of string                   (* Local variant type: event_value atomic_update *)
  | AtomicVariantOption of string             (* Local variant type option: event_value atomic_change *)
  | AtomicVariantList of string               (* Local variant type list: event_value atomic_change list *)
  | StructuredUpdate of Longident.t * Longident.t  (* (full_module, patch_module) - is_unchanged_update (module M.Patch), diff_complex_value (module M) *)
  | StructuredUpdateId of Longident.t * Longident.t  (* (full_module, patch_module) - is_unchanged_update (module M.Patch), diff_complex_value_id (module M) *)
  | StructuredChange of Longident.t * Longident.t  (* (full_module, patch_module) - is_unchanged_change (module M.Patch), diff_complex_value_opt (module M) *)
  | StructuredChangeId of Longident.t * Longident.t  (* (full_module, patch_module) - is_unchanged_change (module M.Patch), diff_complex_value_id_opt (module M) *)
  | StructuredChangeList of Longident.t * Longident.t  (* (full_module, patch_module) - is_unchanged_change (module M.Patch), diff_list_id (module M) *)
  | Identity                                   (* field in Patch.t but no is_unchanged check (for traceability) *)

type patch_field_info = {
  field_name: string;
  patch_type: core_type;
  check_kind: check_kind;
}

(** Atomic types with their corresponding module names *)
type atomic_type =
  | Int
  | Float
  | String
  | Bool

(** Map string type name to atomic_type *)
let string_to_atomic_type = function
  | "int" -> Some Int
  | "float" -> Some Float
  | "string" -> Some String
  | "bool" -> Some Bool
  | _ -> None

(** Get module name for atomic type *)
let atomic_module_name = function
  | Int -> "Int"
  | Float -> "Float"
  | String -> "String"
  | Bool -> "Bool"

(** Create module expression for atomic type *)
let atomic_module_expr ~loc atomic_type =
  let open Ast_builder.Default in
  let mod_name = atomic_module_name atomic_type in
  pexp_pack ~loc (pmod_ident ~loc { txt = Lident mod_name; loc })

(** Extract atomic type from a core_type *)
let extract_atomic_type_from_type (ptyp: core_type) : atomic_type option =
  match ptyp.ptyp_desc with
  | Ptyp_constr ({ txt = Lident name; _ }, []) -> string_to_atomic_type name
  | _ -> None

(** Extract atomic type from a patch type (e.g., atomic_update int) *)
let extract_atomic_type_from_patch_type (ptyp: core_type) : atomic_type option =
  match ptyp.ptyp_desc with
  | Ptyp_constr ({ txt = Lident ("atomic_update" | "atomic_change"); _ }, [arg]) ->
    extract_atomic_type_from_type arg
  | Ptyp_constr ({ txt = Lident "list"; _ }, [arg]) ->
    (match arg.ptyp_desc with
     | Ptyp_constr ({ txt = Lident "atomic_change"; _ }, [inner]) ->
       extract_atomic_type_from_type inner
     | _ -> None)
  | _ -> None

(** Create a module expression from a longident *)
let create_module_expr ~loc (lid : Longident.t) : expression =
  let open Ast_builder.Default in
  pexp_pack ~loc (pmod_ident ~loc { txt = lid; loc })

(** Check if a type is an atomic type (int, float, string, bool) *)
let is_atomic_type : core_type -> bool = function
  | { ptyp_desc = Ptyp_constr ({ txt = Lident ("int" | "float" | "string" | "bool"); _ }, []); _ } -> true
  | _ -> false

(** Get the atomic type name for atomic_change wrapper *)
let get_atomic_type_name (ptyp: core_type) : string =
  match ptyp.ptyp_desc with
  | Ptyp_constr ({ txt = Lident name; _ }, []) -> name
  | _ -> assert false

(** Helper to create a type constructor with a string name *)
let mk_typ_constr_str loc name args =
  let open Ast_builder.Default in
  ptyp_constr ~loc { loc; txt = lident name } args

(** Helper to create a type constructor with a longident *)
let mk_typ_constr_lid loc lid =
  let open Ast_builder.Default in
  ptyp_constr ~loc { loc; txt = lid }

let mk_typ_constr_lid_with_args loc lid args =
  let open Ast_builder.Default in
  ptyp_constr ~loc { loc; txt = lid } args

(** Capitalize first character of a string *)
let capitalize_first (s: string) : string =
  match s with "" -> "" | _ ->
    String.mapi (fun i c -> if i = 0 then Char.uppercase_ascii c else c) s

(** Create module expression for a local equality module: struct type t = <type_name> let equal = (=) end *)
let create_local_eq_module_expr ~loc (type_name: string) : module_expr =
  let open Ast_builder.Default in
  let type_manifest = mk_typ_constr_str loc type_name [] in
  let type_decl =
    { ptype_name = { txt = "t"; loc }
    ; ptype_params = []
    ; ptype_cstrs = []
    ; ptype_kind = Ptype_abstract
    ; ptype_private = Public
    ; ptype_manifest = Some type_manifest
    ; ptype_attributes = []
    ; ptype_loc = loc }
  in
  let type_item = pstr_type ~loc Nonrecursive [type_decl] in
  let equal_expr = pexp_ident ~loc { txt = Lident "="; loc } in
  let equal_binding =
    { pvb_pat = ppat_var ~loc { txt = "equal"; loc }
    ; pvb_expr = equal_expr
    ; pvb_attributes = []
    ; pvb_loc = loc
    ; pvb_constraint = None }
  in
  let equal_item = pstr_value ~loc Nonrecursive [equal_binding] in
  pmod_structure ~loc [type_item; equal_item]

(** Given a longident like "Loop.t" (as Ldot (Lident "Loop", "t")), construct the Patch module type
    as "Loop.Patch.t" (as Ldot (Ldot (Lident "Loop", "Patch"), "t"))

    This handles cases where the type name is a simple identifier like "Loop.t"
    vs a nested path like "Foo.Bar.Type.t"
*)
let patch_module_type_of_type_lid (lid: Longident.t) : Longident.t =
  (* Convert Module.t to Module.Patch.t by replacing the "t" suffix with "Patch.t" *)
  match lid with
  (* Handle Module.t -> Module.Patch.t *)
  | Ldot (prefix, "t") -> Ldot (Ldot (prefix, "Patch"), "t")
  (* Fallback for other cases (including Lident with no dot or deeply nested) *)
  | _ -> Ldot (Ldot (lid, "Patch"), "t")

(** Given a longident like "Loop.t" (as Ldot (Lident "Loop", "t")), construct the Patch module path
    as "Loop.Patch" (as Ldot (Lident "Loop", "Patch"))

    This is used for module references like (module Loop.Patch) in is_unchanged_update calls
*)
let patch_module_of_type_lid (lid: Longident.t) : Longident.t =
  (* Convert Module.t to Module.Patch by replacing the "t" suffix with "Patch" *)
  match lid with
  (* Handle Module.t -> Module.Patch *)
  | Ldot (prefix, "t") -> Ldot (prefix, "Patch")
  (* Fallback for other cases (including Lident with no dot or deeply nested) *)
  | _ -> Ldot (lid, "Patch")

(** Extract the full module name from a type like Loop.t or Foo.Bar.t
    This is used for module references like (module Loop) in diff_complex_value calls
*)
let full_module_of_type_lid (lid: Longident.t) : Longident.t =
  (* Convert Module.t to Module by removing the "t" suffix *)
  match lid with
  (* Handle Module.t -> Module *)
  | Ldot (prefix, "t") -> prefix
  (* Fallback: return as-is for other cases *)
  | _ -> lid

(** Create a structured_update type for a structured type *)
let create_structured_update_type ~loc (lid : Longident.t) : core_type =
  let open Ast_builder.Default in
  let patch_module_type = patch_module_type_of_type_lid lid in
  mk_typ_constr_str loc "structured_update"
    [mk_typ_constr_lid loc patch_module_type []]

(** Create a structured_change type for a structured type option *)
let create_structured_change_type ~loc (lid : Longident.t) : core_type =
  let open Ast_builder.Default in
  let patch_module_type = patch_module_type_of_type_lid lid in
  mk_typ_constr_str loc "structured_change"
    [mk_typ_constr_lid loc lid [];
     mk_typ_constr_lid loc patch_module_type []]

(** Create a structured_change list type for a structured type list *)
let create_structured_change_list_type ~loc (lid : Longident.t) : core_type =
  let open Ast_builder.Default in
  let patch_module_type = patch_module_type_of_type_lid lid in
  mk_typ_constr_str loc "list"
    [mk_typ_constr_str loc "structured_change"
       [mk_typ_constr_lid loc lid [];
        mk_typ_constr_lid loc patch_module_type []]]

(** Generic attribute checker *)
let has_attribute (attributes: attributes) attr_name =
  List.exists attributes ~f:(fun attr ->
      String.equal attr.attr_name.txt attr_name
    )

(** Check if a field has [@patch.skip] attribute *)
let field_has_skip_attr ld = has_attribute ld.pld_attributes "patch.skip"

(** Check if a field has [@patch.identity] attribute *)
let field_has_identity_attr ld = has_attribute ld.pld_attributes "patch.identity"

(** Check if a type declaration has [@@@patch.generate_diff] attribute *)
let type_has_generate_diff_attr td = has_attribute td.ptype_attributes "patch.generate_diff"

(** Check if a field has [@id.id] attribute *)
let field_has_id_attr ld = has_attribute ld.pld_attributes "id.id"

(** Check if a field has [@id.ref] attribute (field's type has identity support) *)
let field_has_id_ref_attr ld = has_attribute ld.pld_attributes "id.ref"

(** Check if a type has any field with [@id.id] attribute (from [@@deriving id]) *)
let type_has_id_field fields =
  List.exists fields ~f:field_has_id_attr

(** Result of classifying a type for patch generation *)
type type_classification = {
  patch_type: core_type;
  check_kind: check_kind;
}

(** Classify a type and return both the patch type and check kind *)
let classify_type (loc: Location.t) (original_type: core_type) : type_classification =
  match original_type.ptyp_desc with
  (* Atomic types -> atomic_update *)
  | Ptyp_constr ({ txt = Lident name; _ }, []) when is_atomic_type original_type ->
    { patch_type = mk_typ_constr_str loc "atomic_update" [mk_typ_constr_str loc name []];
      check_kind = AtomicUpdate }
  (* Atomic type option -> atomic_change *)
  | Ptyp_constr ({ txt = Lident "option"; _ }, [arg]) when is_atomic_type arg ->
    let name = get_atomic_type_name arg in
    { patch_type = mk_typ_constr_str loc "atomic_change" [mk_typ_constr_str loc name []];
      check_kind = AtomicChange }
  (* Atomic type list -> <type> atomic_change list *)
  | Ptyp_constr ({ txt = Lident "list"; _ }, [arg]) when is_atomic_type arg ->
    let name = get_atomic_type_name arg in
    { patch_type = mk_typ_constr_str loc "list" [mk_typ_constr_str loc "atomic_change" [mk_typ_constr_str loc name []]];
      check_kind = AtomicChangeList }
  (* Structured type list -> (Foo.t, Foo.Patch.t) structured_change list *)
  | Ptyp_constr ({ txt = Lident "list"; _ }, [arg]) ->
    (match arg.ptyp_desc with
     | Ptyp_constr ({ txt = Lident name; _ }, []) when not (is_atomic_type arg) ->
       { patch_type = mk_typ_constr_str loc "list"
             [mk_typ_constr_str loc "atomic_change" [mk_typ_constr_str loc name []]];
         check_kind = AtomicVariantList name }
     | Ptyp_constr ({ txt = lid; _ }, []) ->
       let full_module = full_module_of_type_lid lid in
       let patch_module = patch_module_of_type_lid lid in
       { patch_type = create_structured_change_list_type ~loc lid;
         check_kind = StructuredChangeList (full_module, patch_module) }
     | _ -> Location.raise_errorf ~loc "Unsupported type in list: %a" Pprintast.core_type arg)
  (* Local variant type -> atomic_update (non-builtin Lident) *)
  | Ptyp_constr ({ txt = Lident name; _ }, []) ->
    { patch_type = mk_typ_constr_str loc "atomic_update" [mk_typ_constr_str loc name []];
      check_kind = AtomicVariant name }
  (* Module-qualified structured type Foo.t -> Foo.Patch.t structured_update *)
  | Ptyp_constr ({ txt = lid; _ }, []) ->
    let full_module = full_module_of_type_lid lid in
    let patch_module = patch_module_of_type_lid lid in
    { patch_type = create_structured_update_type ~loc lid;
      check_kind = StructuredUpdate (full_module, patch_module) }
  (* Structured type Foo.t option -> (Foo.t, Foo.Patch.t) structured_change *)
  | Ptyp_constr ({ txt = Lident "option"; _ }, [arg]) ->
    (match arg.ptyp_desc with
     | Ptyp_constr ({ txt = Lident name; _ }, []) when not (is_atomic_type arg) ->
       { patch_type = mk_typ_constr_str loc "atomic_change" [mk_typ_constr_str loc name []];
         check_kind = AtomicVariantOption name }
     | Ptyp_constr ({ txt = lid; _ }, []) ->
       let full_module = full_module_of_type_lid lid in
       let patch_module = patch_module_of_type_lid lid in
       { patch_type = create_structured_change_type ~loc lid;
         check_kind = StructuredChange (full_module, patch_module) }
     | _ -> Location.raise_errorf ~loc "Unsupported type in option: %a" Pprintast.core_type arg)
  | _ -> Location.raise_errorf ~loc "Unsupported type for patch derivation: %a" Pprintast.core_type original_type

(** Classify a type and return the corresponding Patch.t field type as an AST *)
let classify_patch_type (loc: Location.t) (ptyp: core_type) : core_type =
  let { patch_type; check_kind = _ } = classify_type loc ptyp in
  patch_type

(** Promote a check_kind to use ID-based diffing when [@id.ref] is present *)
let promote_to_id ~loc ~original_type = function
  | StructuredUpdate (m, p) -> StructuredUpdateId (m, p)
  | StructuredChange (m, p) -> StructuredChangeId (m, p)
  | StructuredChangeList _ ->
    Location.raise_errorf ~loc
      "[@id.ref] is redundant on list fields (list diffing already uses ID-based matching)"
  | _ ->
    Location.raise_errorf ~loc
      "[@id.ref] can only be used on structured types, not on %a"
      Pprintast.core_type original_type

(** Classify a field and return both the patch type and the check kind *)
(* Returns None if the field has [@patch.skip] attribute *)
(* Returns Some with Identity check_kind if field has [@patch.identity] attribute *)
let classify_patch_field (loc: Location.t) (ld: label_declaration) : patch_field_info option =
  let field_name = ld.pld_name.txt in
  let original_type = ld.pld_type in

  (* Check for [@patch.identity] - include field in Patch.t as original type (not wrapped) *)
  if field_has_identity_attr ld then
    Some { field_name; patch_type = original_type; check_kind = Identity }
  else if field_has_skip_attr ld then
    None
  else
    let { patch_type; check_kind } = classify_type loc original_type in
    let check_kind =
      if field_has_id_ref_attr ld then promote_to_id ~loc ~original_type check_kind
      else check_kind
    in
    Some { field_name; patch_type; check_kind }

(** Generate the diffing expression for a single constructor arg (positional, not named) *)
let generate_arg_diff ~loc old_expr new_expr (patch_type: core_type) (check_kind: check_kind) : expression =
  let open Ast_builder.Default in
  match check_kind with
  | AtomicUpdate ->
    let atomic_type = match extract_atomic_type_from_patch_type patch_type with
      | Some t -> t
      | None -> Location.raise_errorf ~loc "Could not extract atomic type from patch type" in
    let mod_pack = atomic_module_expr ~loc atomic_type in
    let diff_fn = pexp_ident ~loc { txt = Lident "diff_atomic_value"; loc } in
    pexp_apply ~loc diff_fn [Nolabel, mod_pack; Nolabel, old_expr; Nolabel, new_expr]

  | StructuredUpdate (full_module, _patch_module) ->
    let module_expr = create_module_expr ~loc full_module in
    let diff_fn = pexp_ident ~loc { txt = Lident "diff_complex_value"; loc } in
    pexp_apply ~loc diff_fn [Nolabel, module_expr; Nolabel, old_expr; Nolabel, new_expr]

  | StructuredUpdateId (full_module, _patch_module) ->
    let module_expr = create_module_expr ~loc full_module in
    let diff_fn = pexp_ident ~loc { txt = Lident "diff_complex_value_id"; loc } in
    pexp_apply ~loc diff_fn [Nolabel, module_expr; Nolabel, old_expr; Nolabel, new_expr]

  | StructuredChange (full_module, _patch_module) ->
    let module_expr = create_module_expr ~loc full_module in
    let diff_fn = pexp_ident ~loc { txt = Lident "diff_complex_value_opt"; loc } in
    pexp_apply ~loc diff_fn [Nolabel, module_expr; Nolabel, old_expr; Nolabel, new_expr]

  | StructuredChangeId (full_module, _patch_module) ->
    let module_expr = create_module_expr ~loc full_module in
    let diff_fn = pexp_ident ~loc { txt = Lident "diff_complex_value_id_opt"; loc } in
    pexp_apply ~loc diff_fn [Nolabel, module_expr; Nolabel, old_expr; Nolabel, new_expr]

  | AtomicChange ->
    let atomic_type = match extract_atomic_type_from_patch_type patch_type with
      | Some t -> t
      | None -> Location.raise_errorf ~loc "Could not extract atomic type from patch type" in
    let mod_pack = atomic_module_expr ~loc atomic_type in
    let diff_fn = pexp_ident ~loc { txt = Lident "diff_atomic_value_opt"; loc } in
    pexp_apply ~loc diff_fn [Nolabel, mod_pack; Nolabel, old_expr; Nolabel, new_expr]

  | AtomicChangeList ->
    let atomic_type = match extract_atomic_type_from_patch_type patch_type with
      | Some t -> t
      | None -> Location.raise_errorf ~loc "Could not extract atomic type from patch type" in
    let mod_name = atomic_module_name atomic_type in
    let mod_pack = atomic_module_expr ~loc atomic_type in
    let diff_fn = pexp_ident ~loc { txt = Lident "diff_list_generic"; loc } in
    let compare_fn = pexp_field ~loc (pexp_ident ~loc { txt = Lident mod_name; loc }) { txt = Lident "equal"; loc } in
    let pat_old = ppat_var ~loc { txt = "old_elem"; loc } in
    let pat_new = ppat_var ~loc { txt = "new_elem"; loc } in
    let access_old = pexp_ident ~loc { txt = Lident "old_elem"; loc } in
    let access_new = pexp_ident ~loc { txt = Lident "new_elem"; loc } in
    let diff_atomic_call = pexp_apply ~loc (pexp_ident ~loc { txt = Lident "diff_atomic_value"; loc })
        [Nolabel, mod_pack; Nolabel, access_old; Nolabel, access_new] in
    let on_match_lambda = pexp_fun ~loc Nolabel None pat_old
        (pexp_fun ~loc Nolabel None pat_new diff_atomic_call) in
    pexp_apply ~loc diff_fn
      [Labelled "compare", compare_fn; Labelled "on_match", on_match_lambda; Nolabel, old_expr; Nolabel, new_expr]

  | StructuredChangeList (full_module, patch_module) ->
    let full_module_expr = create_module_expr ~loc full_module in
    let patch_module_expr = create_module_expr ~loc patch_module in
    let diff_list_fn = pexp_ident ~loc { txt = Lident "diff_list_id"; loc } in
    let diff_list_call = pexp_apply ~loc diff_list_fn [Nolabel, full_module_expr; Nolabel, old_expr; Nolabel, new_expr] in
    let filter_fn = pexp_ident ~loc { txt = Lident "filter_changes"; loc } in
    let filter_call = pexp_apply ~loc filter_fn [Nolabel, patch_module_expr] in
    pexp_apply ~loc (pexp_ident ~loc { txt = Lident "|>"; loc }) [Nolabel, diff_list_call; Nolabel, filter_call]

  | Identity ->
    new_expr

  | AtomicVariant type_name ->
    let mod_name = capitalize_first type_name ^ "_eq" in
    let mod_expr = create_local_eq_module_expr ~loc type_name in
    let mod_pack = pexp_pack ~loc (pmod_ident ~loc { txt = Lident mod_name; loc }) in
    let diff_fn = pexp_ident ~loc { txt = Lident "diff_atomic_value"; loc } in
    let diff_call = pexp_apply ~loc diff_fn [Nolabel, mod_pack; Nolabel, old_expr; Nolabel, new_expr] in
    pexp_letmodule ~loc { txt = Some mod_name; loc } mod_expr diff_call

  | AtomicVariantOption type_name ->
    let mod_name = capitalize_first type_name ^ "_eq" in
    let mod_expr = create_local_eq_module_expr ~loc type_name in
    let mod_pack = pexp_pack ~loc (pmod_ident ~loc { txt = Lident mod_name; loc }) in
    let diff_fn = pexp_ident ~loc { txt = Lident "diff_atomic_value_opt"; loc } in
    let diff_call = pexp_apply ~loc diff_fn [Nolabel, mod_pack; Nolabel, old_expr; Nolabel, new_expr] in
    pexp_letmodule ~loc { txt = Some mod_name; loc } mod_expr diff_call

  | AtomicVariantList type_name ->
    let mod_name = capitalize_first type_name ^ "_eq" in
    let mod_expr = create_local_eq_module_expr ~loc type_name in
    let diff_fn = pexp_ident ~loc { txt = Lident "diff_list_generic"; loc } in
    let compare_fn = pexp_field ~loc (pexp_ident ~loc { txt = Lident mod_name; loc }) { txt = Lident "equal"; loc } in
    let pat_old = ppat_var ~loc { txt = "old_elem"; loc } in
    let pat_new = ppat_var ~loc { txt = "new_elem"; loc } in
    let access_old = pexp_ident ~loc { txt = Lident "old_elem"; loc } in
    let access_new = pexp_ident ~loc { txt = Lident "new_elem"; loc } in
    let mod_pack_inner = pexp_pack ~loc (pmod_ident ~loc { txt = Lident mod_name; loc }) in
    let diff_atomic_call = pexp_apply ~loc (pexp_ident ~loc { txt = Lident "diff_atomic_value"; loc })
        [Nolabel, mod_pack_inner; Nolabel, access_old; Nolabel, access_new] in
    let on_match_lambda = pexp_fun ~loc Nolabel None pat_old
        (pexp_fun ~loc Nolabel None pat_new diff_atomic_call) in
    let diff_call = pexp_apply ~loc diff_fn
        [Labelled "compare", compare_fn; Labelled "on_match", on_match_lambda; Nolabel, old_expr; Nolabel, new_expr] in
    pexp_letmodule ~loc { txt = Some mod_name; loc } mod_expr diff_call

(** Generate the is_unchanged_* check expression for a constructor arg *)
let generate_arg_check ~loc (arg_expr: expression) (check_kind: check_kind) : expression =
  let open Ast_builder.Default in
  match check_kind with
  | AtomicUpdate ->
    [%expr is_unchanged_atomic_update [%e arg_expr]]
  | AtomicChange ->
    [%expr is_unchanged_atomic_change [%e arg_expr]]
  | StructuredUpdate (_full_module, patch_module)
  | StructuredUpdateId (_full_module, patch_module) ->
    let module_expr = create_module_expr ~loc patch_module in
    let check_fn = pexp_ident ~loc { txt = Lident "is_unchanged_update"; loc } in
    pexp_apply ~loc check_fn [Nolabel, module_expr; Nolabel, arg_expr]
  | StructuredChange (_full_module, patch_module)
  | StructuredChangeId (_full_module, patch_module) ->
    let module_expr = create_module_expr ~loc patch_module in
    let check_fn = pexp_ident ~loc { txt = Lident "is_unchanged_change"; loc } in
    pexp_apply ~loc check_fn [Nolabel, module_expr; Nolabel, arg_expr]
  | AtomicChangeList ->
    [%expr List.for_all is_unchanged_atomic_change [%e arg_expr]]
  | StructuredChangeList (_full_module, patch_module) ->
    let for_all = pexp_ident ~loc { txt = Ldot (Lident "List", "for_all"); loc } in
    let module_expr = create_module_expr ~loc patch_module in
    let check_fn = pexp_ident ~loc { txt = Lident "is_unchanged_change"; loc } in
    let pat_x = ppat_var ~loc { txt = "x"; loc } in
    let exp_x = pexp_ident ~loc { txt = Lident "x"; loc } in
    let lambda = pexp_fun ~loc Nolabel None pat_x
        (pexp_apply ~loc check_fn [Nolabel, module_expr; Nolabel, exp_x]) in
    pexp_apply ~loc for_all [Nolabel, lambda; Nolabel, arg_expr]
  | Identity ->
    [%expr true]
  | AtomicVariant _ ->
    [%expr is_unchanged_atomic_update [%e arg_expr]]
  | AtomicVariantOption _ ->
    [%expr is_unchanged_atomic_change [%e arg_expr]]
  | AtomicVariantList _ ->
    [%expr List.for_all is_unchanged_atomic_change [%e arg_expr]]

(** Generate the diffing expression for a single field *)
let generate_field_diff ~loc old_var new_var (fi: patch_field_info) : string * expression =
  let open Ast_builder.Default in
  let old_access = pexp_field ~loc old_var { txt = Lident fi.field_name; loc } in
  let new_access = pexp_field ~loc new_var { txt = Lident fi.field_name; loc } in
  (fi.field_name, generate_arg_diff ~loc old_access new_access fi.patch_type fi.check_kind)

(** Generate the is_unchanged_* check expression for a single field *)
let generate_field_check ~loc (p_var: expression) (field_info: patch_field_info) : expression =
  let open Ast_builder.Default in
  let field_access = pexp_field ~loc p_var { txt = Lident field_info.field_name; loc } in
  generate_arg_check ~loc field_access field_info.check_kind

(** Generate ID validation expression for diff function
    Returns None if no ID field, Some validation expression otherwise
*)
let generate_id_validation ~loc type_name fields old_var new_var : expression option =
  let open Ast_builder.Default in
  (* Find all fields with [@id.id] attribute but NOT [@patch.skip] *)
  let id_fields = List.filter fields ~f:(fun ld ->
      field_has_id_attr ld && not (field_has_skip_attr ld)
    ) in
  match id_fields with
  | [] -> None
  | [id_field] ->
    (* Single ID field - use direct comparison *)
    let field_name = id_field.pld_name.txt in
    let old_id = pexp_field ~loc old_var { txt = Lident field_name; loc } in
    let new_id = pexp_field ~loc new_var { txt = Lident field_name; loc } in
    Some [%expr
      if [%e old_id] <> [%e new_id] then
        failwith (Printf.sprintf "Cannot diff two %s with different Ids" [%e estring ~loc type_name])
      else
        ()
    ]
  | _ ->
    (* Multiple ID fields - fail when ANY identity field differs (OR semantics) *)
    let inequalities = List.map id_fields ~f:(fun id_field ->
        let field_name = id_field.pld_name.txt in
        let old_id = pexp_field ~loc old_var { txt = Lident field_name; loc } in
        let new_id = pexp_field ~loc new_var { txt = Lident field_name; loc } in
        [%expr [%e old_id] <> [%e new_id]]
      ) in
    let any_differ = Ppx_shared.chain_or ~loc inequalities in
    Some [%expr
      if [%e any_differ] then
        failwith (Printf.sprintf "Cannot diff two %s with different Ids" [%e estring ~loc type_name])
      else
        ()
    ]

(** Generate the is_empty function implementation *)
let generate_is_empty_impl ~loc (fields: patch_field_info list) : structure =
  let open Ast_builder.Default in
  (* Generate checks for each field *)
  let p_var = pexp_ident ~loc { txt = Lident "p"; loc } in
  let field_checks = List.map fields ~f:(generate_field_check ~loc p_var) in

  (* Chain all checks with && *)
  let body = Ppx_shared.chain_and ~loc field_checks in

  (* Use metaquot for readable function definition *)
  [%str
    let is_empty p =
      [%e body]
  ]

(** Generate signature for is_empty function *)
let generate_is_empty_sig ~loc : signature =
  [%sig: val is_empty : t -> bool]

(** Generate signature for diff function *)
let generate_diff_sig ~loc : signature =
  [%sig: val diff : t -> t -> Patch.t]

(** Generate the diff function implementation *)
let generate_diff_impl ~ctxt:_ type_decl field_infos =
  let open Ast_builder.Default in
  let loc = type_decl.ptype_loc in
  let type_name = type_decl.ptype_name.txt in

  (* Get fields for ID detection *)
  let fields = match type_decl.ptype_kind with
    | Ptype_record fields -> fields
    | _ -> []
  in

  (* Variable names: old_t and new_t *)
  let old_var = pexp_ident ~loc { txt = Lident "old_t"; loc } in
  let new_var = pexp_ident ~loc { txt = Lident "new_t"; loc } in

  (* Generate field diffing expressions *)
  let field_diffs = List.map field_infos ~f:(fun fi ->
      generate_field_diff ~loc old_var new_var fi
    ) in

  (* Generate body with optional ID validation *)
  let body =
    let id_validation = generate_id_validation ~loc type_name fields old_var new_var in
    (* Convert string field names to longident for pexp_record *)
    let record_fields = List.map field_diffs ~f:(fun (n, e) -> ({ txt = Lident n; loc }, e)) in
    let record_expr = pexp_record ~loc record_fields None in
    match id_validation with
    | None -> record_expr
    | Some validation -> pexp_sequence ~loc validation record_expr
  in

  (* Generate function using meta-quotation *)
  [%str
    let diff (old_t : t) (new_t : t) : Patch.t =
      [%e body]
  ]

(** Classify constructor tuple args and return (name, classification) pairs *)
let classify_constructor_args ~loc args =
  List.mapi args ~f:(fun i arg ->
      let cls = classify_type loc arg in
      (Printf.sprintf "a%d" i, cls)
    )

(** Generate the Patch.t type for a variant *)
let generate_variant_patch_type ~loc constructors =
  let open Ast_builder.Default in
  let patch_constructors = List.map constructors ~f:(fun cd ->
      let new_args = match cd.pcd_args with
        | Pcstr_tuple args ->
          let patch_args = List.map args ~f:(fun arg ->
              (classify_type loc arg).patch_type
            ) in
          Pcstr_tuple patch_args
        | Pcstr_record fields ->
          let patch_fields = List.map fields ~f:(fun ld ->
              let cls = classify_type loc ld.pld_type in
              { ld with pld_type = cls.patch_type }
            ) in
          Pcstr_record patch_fields
      in
      { cd with pcd_args = new_args; pcd_attributes = []; pcd_res = None }
    ) in
  Ast_builder.Default.type_declaration
    ~loc
    ~name:{ txt = "t"; loc }
    ~params:[]
    ~cstrs:[]
    ~kind:(Ptype_variant patch_constructors)
    ~private_:Public
    ~manifest:None

(** Generate the is_empty function for a variant Patch.t *)
let generate_variant_is_empty_impl ~loc constructors =
  let open Ast_builder.Default in
  let p_var = pexp_ident ~loc { txt = Lident "p"; loc } in
  let mk_case (pat, body) = { pc_lhs = pat; pc_guard = None; pc_rhs = body } in
  let cases = List.map constructors ~f:(fun cd ->
      let cstr_name = cd.pcd_name.txt in
      let cstr_lid = { txt = Lident cstr_name; loc } in
      match cd.pcd_args with
      | Pcstr_tuple [] ->
        mk_case (ppat_construct ~loc cstr_lid None, [%expr true])
      | Pcstr_tuple args ->
        let arg_infos = classify_constructor_args ~loc args in
        let arg_pat = match arg_infos with
          | [(name, _)] -> ppat_var ~loc { txt = name; loc }
          | infos -> ppat_tuple ~loc (List.map infos ~f:(fun (name, _) ->
              ppat_var ~loc { txt = name; loc }))
        in
        let pat = ppat_construct ~loc cstr_lid (Some arg_pat) in
        let checks = List.map arg_infos ~f:(fun (name, cls) ->
            let arg_expr = pexp_ident ~loc { txt = Lident name; loc } in
            generate_arg_check ~loc arg_expr cls.check_kind
          ) in
        mk_case (pat, Ppx_shared.chain_and ~loc checks)
      | Pcstr_record fields ->
        let field_infos = List.map fields ~f:(fun ld ->
            let cls = classify_type loc ld.pld_type in
            (ld.pld_name.txt, cls)
          ) in
        let field_pats = List.map field_infos ~f:(fun (name, _) ->
            ({ txt = Lident name; loc }, ppat_var ~loc { txt = "f_" ^ name; loc })
          ) in
        let pat = ppat_construct ~loc cstr_lid
            (Some (ppat_record ~loc field_pats Closed)) in
        let checks = List.map field_infos ~f:(fun (name, cls) ->
            let arg_expr = pexp_ident ~loc { txt = Lident ("f_" ^ name); loc } in
            generate_arg_check ~loc arg_expr cls.check_kind
          ) in
        mk_case (pat, Ppx_shared.chain_and ~loc checks)
    ) in
  let match_expr = pexp_match ~loc p_var cases in
  [%str let is_empty p = [%e match_expr]]

(** Generate the diff function for a variant type *)
let generate_variant_diff_impl ~loc type_name constructors =
  let open Ast_builder.Default in
  let old_var = pexp_ident ~loc { txt = Lident "old_t"; loc } in
  let new_var = pexp_ident ~loc { txt = Lident "new_t"; loc } in
  let mk_case (pat, body) = { pc_lhs = pat; pc_guard = None; pc_rhs = body } in
  let cases = List.map constructors ~f:(fun cd ->
      let cstr_name = cd.pcd_name.txt in
      let cstr_lid = { txt = Lident cstr_name; loc } in
      match cd.pcd_args with
      | Pcstr_tuple [] ->
        let old_pat = ppat_construct ~loc cstr_lid None in
        let new_pat = ppat_construct ~loc cstr_lid None in
        let body = pexp_construct ~loc cstr_lid None in
        mk_case (ppat_tuple ~loc [old_pat; new_pat], body)
      | Pcstr_tuple args ->
        let arg_infos = classify_constructor_args ~loc args in
        let old_arg_pat = match arg_infos with
          | [(name, _)] -> ppat_var ~loc { txt = "old_" ^ name; loc }
          | infos -> ppat_tuple ~loc (List.map infos ~f:(fun (name, _) ->
              ppat_var ~loc { txt = "old_" ^ name; loc }))
        in
        let new_arg_pat = match arg_infos with
          | [(name, _)] -> ppat_var ~loc { txt = "new_" ^ name; loc }
          | infos -> ppat_tuple ~loc (List.map infos ~f:(fun (name, _) ->
              ppat_var ~loc { txt = "new_" ^ name; loc }))
        in
        let old_pat = ppat_construct ~loc cstr_lid (Some old_arg_pat) in
        let new_pat = ppat_construct ~loc cstr_lid (Some new_arg_pat) in
        let diff_exprs = List.mapi arg_infos ~f:(fun _i (name, cls) ->
            let old_expr = pexp_ident ~loc { txt = Lident ("old_" ^ name); loc } in
            let new_expr = pexp_ident ~loc { txt = Lident ("new_" ^ name); loc } in
            generate_arg_diff ~loc old_expr new_expr cls.patch_type cls.check_kind
          ) in
        let result_arg = match diff_exprs with
          | [e] -> e
          | es -> pexp_tuple ~loc es
        in
        let body = pexp_construct ~loc cstr_lid (Some result_arg) in
        mk_case (ppat_tuple ~loc [old_pat; new_pat], body)
      | Pcstr_record fields ->
        let field_infos = List.map fields ~f:(fun ld ->
            let cls = classify_type loc ld.pld_type in
            (ld.pld_name.txt, cls)
          ) in
        let old_field_pats = List.map field_infos ~f:(fun (name, _) ->
            ({ txt = Lident name; loc }, ppat_var ~loc { txt = "old_f_" ^ name; loc })
          ) in
        let new_field_pats = List.map field_infos ~f:(fun (name, _) ->
            ({ txt = Lident name; loc }, ppat_var ~loc { txt = "new_f_" ^ name; loc })
          ) in
        let old_pat = ppat_construct ~loc cstr_lid
            (Some (ppat_record ~loc old_field_pats Closed)) in
        let new_pat = ppat_construct ~loc cstr_lid
            (Some (ppat_record ~loc new_field_pats Closed)) in
        let diff_fields = List.map field_infos ~f:(fun (name, cls) ->
            let old_expr = pexp_ident ~loc { txt = Lident ("old_f_" ^ name); loc } in
            let new_expr = pexp_ident ~loc { txt = Lident ("new_f_" ^ name); loc } in
            ({ txt = Lident name; loc },
             generate_arg_diff ~loc old_expr new_expr cls.patch_type cls.check_kind)
          ) in
        let body = pexp_construct ~loc cstr_lid
            (Some (pexp_record ~loc diff_fields None)) in
        mk_case (ppat_tuple ~loc [old_pat; new_pat], body)
    ) in
  let fail_msg = Printf.sprintf "cannot diff %s with different constructors" type_name in
  let wildcard = mk_case (ppat_tuple ~loc [ppat_any ~loc; ppat_any ~loc],
                          pexp_apply ~loc (pexp_ident ~loc { txt = Lident "failwith"; loc })
                            [Nolabel, estring ~loc fail_msg]) in
  let all_cases = cases @ [wildcard] in
  let match_expr = pexp_match ~loc (pexp_tuple ~loc [old_var; new_var]) all_cases in
  [%str let diff (old_t : t) (new_t : t) : Patch.t = [%e match_expr]]

(** Generate the Patch module containing only the t type *)
(* Returns a list of structure items: the Patch module and optionally the diff function *)
let generate_patch_module ~ctxt type_decl =
  let loc = type_decl.ptype_loc in
  match type_decl.ptype_kind with
  | Ptype_record fields ->
    (* Classify fields and get check info (filter out skipped fields) *)
    let field_infos = List.filter_map fields ~f:(fun ld -> classify_patch_field loc ld) in

    (* Check if diff generation is requested *)
    let has_generate_diff = type_has_generate_diff_attr type_decl in

    (* Generate Patch.t record fields *)
    let patch_fields = List.map field_infos ~f:(fun fi ->
        { pld_name = { txt = fi.field_name; loc };
          pld_mutable = Immutable;
          pld_type = fi.patch_type;
          pld_attributes = [];
          pld_loc = loc; }
      ) in
    (* Create Patch module *)
    let patch_type_decl =
      Ast_builder.Default.type_declaration
        ~loc
        ~name:{ txt = "t"; loc }
        ~params:[]
        ~cstrs:[]
        ~kind:(Ptype_record patch_fields)
        ~private_:Public
        ~manifest:None
    in
    let patch_structure_item = Ast_builder.Default.pstr_type ~loc Nonrecursive [patch_type_decl] in

    (* Generate is_empty function *)
    let is_empty_impl = generate_is_empty_impl ~loc field_infos in

    (* Create Patch module with type and is_empty *)
    let patch_module_contents = patch_structure_item :: is_empty_impl in
    let patch_module_binding =
      { pmb_name = { txt = Some "Patch"; loc };
        pmb_expr = Ast_builder.Default.pmod_structure ~loc patch_module_contents;
        pmb_attributes = [];
        pmb_loc = loc }
    in
    let patch_module = Ast_builder.Default.pstr_module ~loc patch_module_binding in

    (* Generate module-level diff function if attribute is present *)
    let diff_function = if has_generate_diff then
        generate_diff_impl ~ctxt type_decl field_infos
      else
        []
    in

    (* Return Patch module and diff function *)
    patch_module :: diff_function

  | Ptype_variant constructors ->
    let has_generate_diff = type_has_generate_diff_attr type_decl in

    (* Generate Patch.t variant type *)
    let patch_type_decl = generate_variant_patch_type ~loc constructors in
    let patch_structure_item = Ast_builder.Default.pstr_type ~loc Nonrecursive [patch_type_decl] in

    (* Generate is_empty function *)
    let is_empty_impl = generate_variant_is_empty_impl ~loc constructors in

    (* Create Patch module *)
    let patch_module_contents = patch_structure_item :: is_empty_impl in
    let patch_module_binding =
      { pmb_name = { txt = Some "Patch"; loc };
        pmb_expr = Ast_builder.Default.pmod_structure ~loc patch_module_contents;
        pmb_attributes = [];
        pmb_loc = loc }
    in
    let patch_module = Ast_builder.Default.pstr_module ~loc patch_module_binding in

    (* Generate diff function if requested *)
    let diff_function = if has_generate_diff then
        generate_variant_diff_impl ~loc type_decl.ptype_name.txt constructors
      else
        []
    in

    patch_module :: diff_function

  | Ptype_abstract | Ptype_open ->
    Location.raise_errorf ~loc "Cannot derive patch for non-record types"

let generate_impl ~ctxt (_rec_flag, type_declarations) =
  let loc = Expansion_context.Deriver.derived_item_loc ctxt in
  List.map type_declarations ~f:(fun td ->
      match td.ptype_kind with
      | Ptype_record _ | Ptype_variant _ -> generate_patch_module ~ctxt td
      | _ ->
        let ext = Location.error_extensionf ~loc:td.ptype_loc
            "Cannot derive patch for non-record types" in
        [Ast_builder.Default.pstr_extension ~loc ext []]
    )
  |> List.concat

let generate_intf ~ctxt (_rec_flag, type_declarations) =
  let loc = Expansion_context.Deriver.derived_item_loc ctxt in
  (* Generate signature for Patch module *)
  List.map type_declarations ~f:(fun td ->
      match td.ptype_kind with
      | Ptype_record fields ->
        (* Classify fields (filter out skipped fields) *)
        let field_infos = List.filter_map fields ~f:(fun ld -> classify_patch_field ld.pld_loc ld) in

        (* Check if diff generation is requested *)
        let has_generate_diff = type_has_generate_diff_attr td in

        (* Generate Patch.t type signature *)
        let patch_fields = List.map field_infos ~f:(fun fi ->
            { pld_name = { txt = fi.field_name; loc };
              pld_mutable = Immutable;
              pld_type = fi.patch_type;
              pld_attributes = [];
              pld_loc = loc; }
          ) in
        let patch_type_decl =
          Ast_builder.Default.type_declaration
            ~loc
            ~name:{ txt = "t"; loc }
            ~params:[]
            ~cstrs:[]
            ~kind:(Ptype_record patch_fields)
            ~private_:Public
            ~manifest:None
        in
        let patch_sig_item = Ast_builder.Default.psig_type ~loc Nonrecursive [patch_type_decl] in

        (* Generate is_empty signature *)
        let is_empty_sig = generate_is_empty_sig ~loc in

        (* Create module signature with type and is_empty *)
        let module_sig_contents = patch_sig_item :: is_empty_sig in
        let module_sig =
          { pmd_name = { txt = Some "Patch"; loc };
            pmd_type = Ast_builder.Default.pmty_signature ~loc module_sig_contents;
            pmd_attributes = [];
            pmd_loc = loc }
        in
        let patch_module_sig = Ast_builder.Default.psig_module ~loc module_sig in

        (* Generate module-level diff signature if attribute is present *)
        let diff_sig = if has_generate_diff then
            generate_diff_sig ~loc
          else
            []
        in

        (* Return Patch module signature and diff signature *)
        patch_module_sig :: diff_sig

      | Ptype_variant constructors ->
        let has_generate_diff = type_has_generate_diff_attr td in
        let patch_type_decl = generate_variant_patch_type ~loc constructors in
        let patch_sig_item = Ast_builder.Default.psig_type ~loc Nonrecursive [patch_type_decl] in
        let is_empty_sig = generate_is_empty_sig ~loc in
        let module_sig_contents = patch_sig_item :: is_empty_sig in
        let module_sig =
          { pmd_name = { txt = Some "Patch"; loc };
            pmd_type = Ast_builder.Default.pmty_signature ~loc module_sig_contents;
            pmd_attributes = [];
            pmd_loc = loc }
        in
        let patch_module_sig = Ast_builder.Default.psig_module ~loc module_sig in
        let diff_sig = if has_generate_diff then generate_diff_sig ~loc else [] in
        patch_module_sig :: diff_sig

      | _ ->
        let ext = Location.error_extensionf ~loc:td.ptype_loc
            "Cannot derive patch for non-record types" in
        [Ast_builder.Default.psig_extension ~loc ext []]
    )
  |> List.concat

let impl_generator = Deriving.Generator.V2.make_noarg generate_impl
let intf_generator = Deriving.Generator.V2.make_noarg generate_intf

let deriver =
  Deriving.add "patch" ~str_type_decl:impl_generator ~sig_type_decl:intf_generator
