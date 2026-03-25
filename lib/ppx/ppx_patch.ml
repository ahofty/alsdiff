open Ppxlib
module List = ListLabels

(** attribute for each field that using [patch] deriver

    - [patch.skip] skipped omitting in the [Patch.t]
*)
type patch_attribute = Skip | Id | Eq of string

(** Metadata about how to generate the is_unchanged check for a field *)
type check_kind =
  | AtomicUpdate                          (* is_unchanged_atomic_update *)
  | AtomicChange                          (* is_unchanged_atomic_change *)
  | StructuredUpdate of Longident.t       (* is_unchanged_update (module M) *)
  | StructuredChange of Longident.t       (* is_unchanged_change (module M) *)
  | AtomicChangeList                      (* List.for_all is_unchanged_atomic_change *)
  | StructuredChangeList of Longident.t   (* List.for_all (is_unchanged_change (module M)) *)
  | Identity                              (* field in Patch.t but no is_unchanged check (for traceability) *)

type patch_field_info = {
  field_name: string;
  patch_type: core_type;
  check_kind: check_kind;
}

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

(** Check if a field has [@patch.skip] attribute *)
let field_has_skip_attr (ld : label_declaration) : bool =
  List.exists ld.pld_attributes ~f:(fun attr ->
      String.equal attr.attr_name.txt "patch.skip"
    )

(** Check if a field has [@patch.identity] attribute *)
let field_has_identity_attr (ld : label_declaration) : bool =
  List.exists ld.pld_attributes ~f:(fun attr ->
      String.equal attr.attr_name.txt "patch.identity"
    )

(** Classify a type and return the corresponding Patch.t field type as an AST *)
let classify_patch_type (loc: Location.t) (ptyp: core_type) : core_type =
  match ptyp.ptyp_desc with
  (* Atomic types -> atomic_update *)
  | Ptyp_constr ({ txt = Lident "int"; _ }, []) ->
    mk_typ_constr_str loc "atomic_update" [mk_typ_constr_str loc "int" []]
  | Ptyp_constr ({ txt = Lident "float"; _ }, []) ->
    mk_typ_constr_str loc "atomic_update" [mk_typ_constr_str loc "float" []]
  | Ptyp_constr ({ txt = Lident "string"; _ }, []) ->
    mk_typ_constr_str loc "atomic_update" [mk_typ_constr_str loc "string" []]
  | Ptyp_constr ({ txt = Lident "bool"; _ }, []) ->
    mk_typ_constr_str loc "atomic_update" [mk_typ_constr_str loc "bool" []]
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
       let patch_module_type = patch_module_type_of_type_lid lid in
       mk_typ_constr_str loc "list"
         [mk_typ_constr_str loc "structured_change"
            [ mk_typ_constr_lid loc lid [];
              mk_typ_constr_lid loc patch_module_type [] ]]
     | _ -> Location.raise_errorf ~loc "Unsupported type in list: %a" Pprintast.core_type arg)
  (* Structured type Foo.t -> Foo.Patch.t structured_update *)
  | Ptyp_constr ({ txt = lid; _ }, []) ->
    let patch_module_type = patch_module_type_of_type_lid lid in
    mk_typ_constr_str loc "structured_update"
      [mk_typ_constr_lid loc patch_module_type []]
  (* Structured type Foo.t option -> (Foo.t, Foo.Patch.t) structured_change *)
  | Ptyp_constr ({ txt = Lident "option"; _ }, [arg]) ->
    (match arg.ptyp_desc with
     | Ptyp_constr ({ txt = lid; _ }, []) ->
       let patch_module_type = patch_module_type_of_type_lid lid in
       mk_typ_constr_str loc "structured_change"
         [ mk_typ_constr_lid loc lid [];
           mk_typ_constr_lid loc patch_module_type [] ]
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
      | Ptyp_constr ({ txt = Lident "int"; _ }, []) ->
        (mk_typ_constr_str loc "atomic_update" [mk_typ_constr_str loc "int" []], AtomicUpdate)
      | Ptyp_constr ({ txt = Lident "float"; _ }, []) ->
        (mk_typ_constr_str loc "atomic_update" [mk_typ_constr_str loc "float" []], AtomicUpdate)
      | Ptyp_constr ({ txt = Lident "string"; _ }, []) ->
        (mk_typ_constr_str loc "atomic_update" [mk_typ_constr_str loc "string" []], AtomicUpdate)
      | Ptyp_constr ({ txt = Lident "bool"; _ }, []) ->
        (mk_typ_constr_str loc "atomic_update" [mk_typ_constr_str loc "bool" []], AtomicUpdate)
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
           let patch_module_type = patch_module_type_of_type_lid lid in
           let patch_module = patch_module_of_type_lid lid in
           let typ = mk_typ_constr_str loc "list"
               [mk_typ_constr_str loc "structured_change"
                  [ mk_typ_constr_lid loc lid [];
                    mk_typ_constr_lid loc patch_module_type [] ]] in
           (typ, StructuredChangeList patch_module)
         | _ -> Location.raise_errorf ~loc "Unsupported type in list: %a" Pprintast.core_type arg)
      (* Structured type Foo.t -> Foo.Patch.t structured_update *)
      | Ptyp_constr ({ txt = lid; _ }, []) ->
        let patch_module_type = patch_module_type_of_type_lid lid in
        let patch_module = patch_module_of_type_lid lid in
        let typ = mk_typ_constr_str loc "structured_update"
            [mk_typ_constr_lid loc patch_module_type []] in
        (typ, StructuredUpdate patch_module)
      (* Structured type Foo.t option -> (Foo.t, Foo.Patch.t) structured_change *)
      | Ptyp_constr ({ txt = Lident "option"; _ }, [arg]) ->
        (match arg.ptyp_desc with
         | Ptyp_constr ({ txt = lid; _ }, []) ->
           let patch_module_type = patch_module_type_of_type_lid lid in
           let patch_module = patch_module_of_type_lid lid in
           let typ = mk_typ_constr_str loc "structured_change"
               [ mk_typ_constr_lid loc lid [];
                 mk_typ_constr_lid loc patch_module_type [] ] in
           (typ, StructuredChange patch_module)
         | _ -> Location.raise_errorf ~loc "Unsupported type in option: %a" Pprintast.core_type arg)
      | _ -> Location.raise_errorf ~loc "Unsupported type for patch derivation: %a" Pprintast.core_type original_type
    in
    Some { field_name; patch_type; check_kind }

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

  | StructuredUpdate module_lid ->
    (* is_unchanged_update (module Foo.Patch) p.field *)
    (* Use pexp_pack with pmod_ident to create (module Foo.Patch) *)
    let module_expr = pexp_pack ~loc (pmod_ident ~loc { txt = module_lid; loc }) in
    let check_fn = pexp_ident ~loc { txt = Lident "is_unchanged_update"; loc } in
    pexp_apply ~loc check_fn [Nolabel, module_expr; Nolabel, field_access]

  | StructuredChange module_lid ->
    (* is_unchanged_change (module Foo.Patch) p.field *)
    let module_expr = pexp_pack ~loc (pmod_ident ~loc { txt = module_lid; loc }) in
    let check_fn = pexp_ident ~loc { txt = Lident "is_unchanged_change"; loc } in
    pexp_apply ~loc check_fn [Nolabel, module_expr; Nolabel, field_access]

  | AtomicChangeList ->
    (* List.for_all is_unchanged_atomic_change p.field *)
    [%expr List.for_all is_unchanged_atomic_change [%e field_access]]

  | StructuredChangeList module_lid ->
    (* List.for_all (fun x -> is_unchanged_change (module Foo.Patch) x) p.field *)
    let for_all = pexp_ident ~loc { txt = Ldot (Lident "List", "for_all"); loc } in
    let module_expr = pexp_pack ~loc (pmod_ident ~loc { txt = module_lid; loc }) in
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

(** Generate the Patch module containing only the t type *)
let generate_patch_module ~ctxt:_ type_decl =
  let loc = type_decl.ptype_loc in
  match type_decl.ptype_kind with
  | Ptype_record fields ->
    (* Classify fields and get check info (filter out skipped fields) *)
    let field_infos = List.filter_map fields ~f:(fun ld -> classify_patch_field loc ld) in

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

    (* Create Patch module with both type and function *)
    let module_binding =
      { pmb_name = { txt = Some "Patch"; loc };
        pmb_expr = Ast_builder.Default.pmod_structure ~loc (patch_structure_item :: is_empty_impl);
        pmb_attributes = [];
        pmb_loc = loc }
    in
    Ast_builder.Default.pstr_module ~loc module_binding
  | Ptype_abstract | Ptype_variant _ | Ptype_open ->
    Location.raise_errorf ~loc "Cannot derive patch for non-record types"

let generate_impl ~ctxt (_rec_flag, type_declarations) =
  let loc = Expansion_context.Deriver.derived_item_loc ctxt in
  List.map type_declarations ~f:(fun td ->
      match td.ptype_kind with
      | Ptype_record _ -> [generate_patch_module ~ctxt td]
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

        (* Create module signature with both type and function *)
        let module_sig =
          { pmd_name = { txt = Some "Patch"; loc };
            pmd_type = Ast_builder.Default.pmty_signature ~loc (patch_sig_item :: is_empty_sig);
            pmd_attributes = [];
            pmd_loc = loc }
        in
        [Ast_builder.Default.psig_module ~loc module_sig]
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
