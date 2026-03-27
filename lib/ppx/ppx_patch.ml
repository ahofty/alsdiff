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
  | StructuredUpdate of Longident.t * Longident.t  (* (full_module, patch_module) - is_unchanged_update (module M.Patch), diff_complex_value (module M) *)
  | StructuredChange of Longident.t * Longident.t  (* (full_module, patch_module) - is_unchanged_change (module M.Patch), diff_complex_value_opt (module M) *)
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

(** Check if a type has any field with [@id.id] attribute (from [@@deriving id]) *)
let type_has_id_field fields =
  List.exists fields ~f:field_has_id_attr

(** Classify a type and return the corresponding Patch.t field type as an AST *)
let classify_patch_type (loc: Location.t) (ptyp: core_type) : core_type =
  match ptyp.ptyp_desc with
  (* Atomic types -> atomic_update *)
  | Ptyp_constr ({ txt = Lident name; _ }, []) when is_atomic_type ptyp ->
    mk_typ_constr_str loc "atomic_update" [mk_typ_constr_str loc name []]
  (* Atomic type option -> atomic_change *)
  | Ptyp_constr ({ txt = Lident "option"; _ }, [arg]) when is_atomic_type arg ->
    let name = get_atomic_type_name arg in
    mk_typ_constr_str loc "atomic_change" [mk_typ_constr_str loc name []]
  (* Atomic type list -> <type> atomic_change list *)
  | Ptyp_constr ({ txt = Lident "list"; _ }, [arg]) when is_atomic_type arg ->
    let name = get_atomic_type_name arg in
    mk_typ_constr_str loc "list" [mk_typ_constr_str loc "atomic_change" [mk_typ_constr_str loc name []]]
  (* Structured type list -> (Foo.t, Foo.Patch.t) structured_change list *)
  | Ptyp_constr ({ txt = Lident "list"; _ }, [arg]) ->
    (match arg.ptyp_desc with
     | Ptyp_constr ({ txt = lid; _ }, []) ->
       create_structured_change_list_type ~loc lid
     | _ -> Location.raise_errorf ~loc "Unsupported type in list: %a" Pprintast.core_type arg)
  (* Structured type Foo.t -> Foo.Patch.t structured_update *)
  | Ptyp_constr ({ txt = lid; _ }, []) ->
    create_structured_update_type ~loc lid
  (* Structured type Foo.t option -> (Foo.t, Foo.Patch.t) structured_change *)
  | Ptyp_constr ({ txt = Lident "option"; _ }, [arg]) ->
    (match arg.ptyp_desc with
     | Ptyp_constr ({ txt = lid; _ }, []) ->
       create_structured_change_type ~loc lid
     | _ -> Location.raise_errorf ~loc "Unsupported type in option: %a" Pprintast.core_type arg)
  | _ -> Location.raise_errorf ~loc "Unsupported type for patch derivation: %a" Pprintast.core_type ptyp

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
    let patch_type, check_kind = match original_type.ptyp_desc with
      (* Atomic types -> atomic_update *)
      | Ptyp_constr ({ txt = Lident name; _ }, []) when is_atomic_type original_type ->
        (mk_typ_constr_str loc "atomic_update" [mk_typ_constr_str loc name []], AtomicUpdate)
      (* Atomic type option -> atomic_change *)
      | Ptyp_constr ({ txt = Lident "option"; _ }, [arg]) when is_atomic_type arg ->
        let name = get_atomic_type_name arg in
        (mk_typ_constr_str loc "atomic_change" [mk_typ_constr_str loc name []], AtomicChange)
      (* Atomic type list -> <type> atomic_change list *)
      | Ptyp_constr ({ txt = Lident "list"; _ }, [arg]) when is_atomic_type arg ->
        let name = get_atomic_type_name arg in
        (mk_typ_constr_str loc "list" [mk_typ_constr_str loc "atomic_change" [mk_typ_constr_str loc name []]], AtomicChangeList)
      (* Structured type list -> (Foo.t, Foo.Patch.t) structured_change list *)
      | Ptyp_constr ({ txt = Lident "list"; _ }, [arg]) ->
        (match arg.ptyp_desc with
         | Ptyp_constr ({ txt = lid; _ }, []) ->
           let full_module = full_module_of_type_lid lid in
           let patch_module = patch_module_of_type_lid lid in
           let typ = create_structured_change_list_type ~loc lid in
           (typ, StructuredChangeList (full_module, patch_module))
         | _ -> Location.raise_errorf ~loc "Unsupported type in list: %a" Pprintast.core_type arg)
      (* Structured type Foo.t -> Foo.Patch.t structured_update *)
      | Ptyp_constr ({ txt = lid; _ }, []) ->
        let full_module = full_module_of_type_lid lid in
        let patch_module = patch_module_of_type_lid lid in
        let typ = create_structured_update_type ~loc lid in
        (typ, StructuredUpdate (full_module, patch_module))
      (* Structured type Foo.t option -> (Foo.t, Foo.Patch.t) structured_change *)
      | Ptyp_constr ({ txt = Lident "option"; _ }, [arg]) ->
        (match arg.ptyp_desc with
         | Ptyp_constr ({ txt = lid; _ }, []) ->
           let full_module = full_module_of_type_lid lid in
           let patch_module = patch_module_of_type_lid lid in
           let typ = create_structured_change_type ~loc lid in
           (typ, StructuredChange (full_module, patch_module))
         | _ -> Location.raise_errorf ~loc "Unsupported type in option: %a" Pprintast.core_type arg)
      | _ -> Location.raise_errorf ~loc "Unsupported type for patch derivation: %a" Pprintast.core_type original_type
    in
    Some { field_name; patch_type; check_kind }

(** Generate the diffing expression for a single field *)
let generate_field_diff ~loc old_var new_var (fi: patch_field_info) : string * expression =
  let open Ast_builder.Default in
  let field_name = fi.field_name in
  let old_access = pexp_field ~loc old_var { txt = Lident field_name; loc } in
  let new_access = pexp_field ~loc new_var { txt = Lident field_name; loc } in

  match fi.check_kind with
  | AtomicUpdate ->
    (* diff_atomic_value (module Type) old_field new_field *)
    (* Extract the inner atomic type from atomic_update wrapper *)
    let atomic_type = match extract_atomic_type_from_patch_type fi.patch_type with
      | Some t -> t
      | None -> Location.raise_errorf ~loc "Could not extract atomic type from patch type" in
    let mod_pack = atomic_module_expr ~loc atomic_type in
    let diff_fn = pexp_ident ~loc { txt = Lident "diff_atomic_value"; loc } in
    let diff_call = pexp_apply ~loc diff_fn [Nolabel, mod_pack; Nolabel, old_access; Nolabel, new_access] in
    (field_name, diff_call)

  | StructuredUpdate (full_module, _patch_module) ->
    (* diff_complex_value (module Type) old_field new_field *)
    let module_expr = create_module_expr ~loc full_module in
    let diff_fn = pexp_ident ~loc { txt = Lident "diff_complex_value"; loc } in
    let diff_call = pexp_apply ~loc diff_fn [Nolabel, module_expr; Nolabel, old_access; Nolabel, new_access] in
    (field_name, diff_call)

  | StructuredChange (full_module, _patch_module) ->
    (* diff_complex_value_opt (module Type) old_field new_field *)
    let module_expr = create_module_expr ~loc full_module in
    let diff_fn = pexp_ident ~loc { txt = Lident "diff_complex_value_opt"; loc } in
    let diff_call = pexp_apply ~loc diff_fn [Nolabel, module_expr; Nolabel, old_access; Nolabel, new_access] in
    (field_name, diff_call)

  | AtomicChange ->
    (* diff_atomic_value_opt (module Type) old_field new_field *)
    let atomic_type = match extract_atomic_type_from_patch_type fi.patch_type with
      | Some t -> t
      | None -> Location.raise_errorf ~loc "Could not extract atomic type from patch type" in
    let mod_pack = atomic_module_expr ~loc atomic_type in
    let diff_fn = pexp_ident ~loc { txt = Lident "diff_atomic_value_opt"; loc } in
    let diff_call = pexp_apply ~loc diff_fn [Nolabel, mod_pack; Nolabel, old_access; Nolabel, new_access] in
    (field_name, diff_call)

  | AtomicChangeList ->
    (* diff_list_generic with ~compare and ~on_match for atomic changes *)
    (* Extract the atomic type from the patch type *)
    let atomic_type = match extract_atomic_type_from_patch_type fi.patch_type with
      | Some t -> t
      | None -> Location.raise_errorf ~loc "Could not extract atomic type from patch type" in
    let mod_name = atomic_module_name atomic_type in
    (* Build module expressions *)
    let mod_pack = atomic_module_expr ~loc atomic_type in
    let diff_fn = pexp_ident ~loc { txt = Lident "diff_list_generic"; loc } in
    (* Build ~compare:Module.equal *)
    let compare_fn = pexp_field ~loc (pexp_ident ~loc { txt = Lident mod_name; loc }) { txt = Lident "equal"; loc } in
    (* Build ~on_match:(fun old new -> diff_atomic_value (module M) old new) *)
    let pat_old = ppat_var ~loc { txt = "old_elem"; loc } in
    let pat_new = ppat_var ~loc { txt = "new_elem"; loc } in
    let access_old = pexp_ident ~loc { txt = Lident "old_elem"; loc } in
    let access_new = pexp_ident ~loc { txt = Lident "new_elem"; loc } in
    let diff_atomic_call = pexp_apply ~loc (pexp_ident ~loc { txt = Lident "diff_atomic_value"; loc })
        [Nolabel, mod_pack; Nolabel, access_old; Nolabel, access_new] in
    let on_match_lambda = pexp_fun ~loc Nolabel None pat_old
        (pexp_fun ~loc Nolabel None pat_new diff_atomic_call) in
    (* Apply diff_list_generic with labeled arguments *)
    let diff_call = pexp_apply ~loc diff_fn
        [Labelled "compare", compare_fn; Labelled "on_match", on_match_lambda; Nolabel, old_access; Nolabel, new_access] in
    (field_name, diff_call)

  | StructuredChangeList (full_module, patch_module) ->
    (* diff_list_id (module Type) old_field new_field |> filter_changes (module Type.Patch) *)
    (* full_module is the full module (e.g., MidiNote), patch_module is the Patch submodule (e.g., MidiNote.Patch) *)
    let full_module_expr = create_module_expr ~loc full_module in
    let patch_module_expr = create_module_expr ~loc patch_module in
    (* Build the pipeline: diff_list_id ... |> filter_changes ... *)
    let diff_list_fn = pexp_ident ~loc { txt = Lident "diff_list_id"; loc } in
    let diff_list_call = pexp_apply ~loc diff_list_fn [Nolabel, full_module_expr; Nolabel, old_access; Nolabel, new_access] in
    let filter_fn = pexp_ident ~loc { txt = Lident "filter_changes"; loc } in
    let filter_call = pexp_apply ~loc filter_fn [Nolabel, patch_module_expr] in
    (* Build the pipe operator application *)
    let diff_call = pexp_apply ~loc (pexp_ident ~loc { txt = Lident "|>"; loc }) [Nolabel, diff_list_call; Nolabel, filter_call] in
    (field_name, diff_call)

  | Identity ->
    (* Copy from new_value (no diffing) *)
    (field_name, new_access)

(** Generate the is_unchanged_* check expression for a single field *)
let generate_field_check ~loc (p_var: expression) (field_info: patch_field_info) : expression =
  let open Ast_builder.Default in
  (* p.field_name *)
  let field_access = pexp_field ~loc p_var { txt = Lident field_info.field_name; loc } in

  match field_info.check_kind with
  | AtomicUpdate ->
    (* is_unchanged_atomic_update p.field *)
    [%expr is_unchanged_atomic_update [%e field_access]]

  | AtomicChange ->
    (* is_unchanged_atomic_change p.field *)
    [%expr is_unchanged_atomic_change [%e field_access]]

  | StructuredUpdate (_full_module, patch_module) ->
    (* is_unchanged_update (module Foo.Patch) p.field *)
    (* Use pexp_pack with pmod_ident to create (module Foo.Patch) *)
    let module_expr = create_module_expr ~loc patch_module in
    let check_fn = pexp_ident ~loc { txt = Lident "is_unchanged_update"; loc } in
    pexp_apply ~loc check_fn [Nolabel, module_expr; Nolabel, field_access]

  | StructuredChange (_full_module, patch_module) ->
    (* is_unchanged_change (module Foo.Patch) p.field *)
    let module_expr = create_module_expr ~loc patch_module in
    let check_fn = pexp_ident ~loc { txt = Lident "is_unchanged_change"; loc } in
    pexp_apply ~loc check_fn [Nolabel, module_expr; Nolabel, field_access]

  | AtomicChangeList ->
    (* List.for_all is_unchanged_atomic_change p.field *)
    [%expr List.for_all is_unchanged_atomic_change [%e field_access]]

  | StructuredChangeList (_full_module, patch_module) ->
    (* List.for_all (fun x -> is_unchanged_change (module Foo.Patch) x) p.field *)
    let for_all = pexp_ident ~loc { txt = Ldot (Lident "List", "for_all"); loc } in
    let module_expr = create_module_expr ~loc patch_module in
    let check_fn = pexp_ident ~loc { txt = Lident "is_unchanged_change"; loc } in
    let pat_x = ppat_var ~loc { txt = "x"; loc } in
    let exp_x = pexp_ident ~loc { txt = Lident "x"; loc } in
    let lambda = pexp_fun ~loc Nolabel None pat_x
        (pexp_apply ~loc check_fn [Nolabel, module_expr; Nolabel, exp_x]) in
    pexp_apply ~loc for_all [Nolabel, lambda; Nolabel, field_access]

  | Identity ->
    (* No check needed - identity fields are always "unchanged" for is_empty *)
    (* They carry identity info but don't represent changes *)
    [%expr true]

(** Chain multiple expressions with && operator *)
let chain_and ~loc (exprs: expression list) : expression =
  match exprs with
  | [] -> [%expr true]
  | [single] -> single
  | first :: rest ->
    List.fold_left rest ~init:first ~f:(fun acc expr ->
        [%expr [%e acc] && [%e expr]]
      )

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
    (* Multiple ID fields - use Id.has_same_id *)
    Some [%expr
      if not (Id.has_same_id [%e old_var] [%e new_var]) then
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
  let body = chain_and ~loc field_checks in

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
  | Ptype_abstract | Ptype_variant _ | Ptype_open ->
    Location.raise_errorf ~loc "Cannot derive patch for non-record types"

let generate_impl ~ctxt (_rec_flag, type_declarations) =
  let loc = Expansion_context.Deriver.derived_item_loc ctxt in
  List.map type_declarations ~f:(fun td ->
      match td.ptype_kind with
      | Ptype_record _ -> generate_patch_module ~ctxt td
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
