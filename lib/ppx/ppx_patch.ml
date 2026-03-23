open Ppxlib
module List = ListLabels

(** attribute for each field that using [patch] deriver

    - [patch.skip] skipped omitting in the [Patch.t]
*)
type patch_attribute = Skip | Id | Eq of string

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
  Ast_helper.Typ.constr ~loc { loc; txt = lident name } args

(** Helper to create a type constructor with a longident *)
let mk_typ_constr_lid loc lid =
  Ast_helper.Typ.constr ~loc { loc; txt = lid }

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
       let patch_module = Ldot (lid, "Patch") in
       mk_typ_constr_str loc "list"
         [mk_typ_constr_str loc "structured_change"
            [ mk_typ_constr_lid loc lid [mk_typ_constr_str loc "t" []];
              mk_typ_constr_lid loc patch_module [mk_typ_constr_str loc "t" []] ]]
     | _ -> Location.raise_errorf ~loc "Unsupported type in list: %a" Pprintast.core_type arg)
  (* Structured type Foo.t -> Foo.Patch.t structured_update *)
  | Ptyp_constr ({ txt = lid; _ }, []) ->
    let patch_module = Ldot (lid, "Patch") in
    mk_typ_constr_str loc "structured_update"
      [mk_typ_constr_lid loc patch_module [mk_typ_constr_str loc "t" []]]
  (* Structured type Foo.t option -> (Foo.t, Foo.Patch.t) structured_change *)
  | Ptyp_constr ({ txt = Lident "option"; _ }, [arg]) ->
    (match arg.ptyp_desc with
     | Ptyp_constr ({ txt = lid; _ }, []) ->
       let patch_module = Ldot (lid, "Patch") in
       mk_typ_constr_str loc "structured_change"
         [ mk_typ_constr_lid loc lid [mk_typ_constr_str loc "t" []];
           mk_typ_constr_lid loc patch_module [mk_typ_constr_str loc "t" []] ]
     | _ -> Location.raise_errorf ~loc "Unsupported type in option: %a" Pprintast.core_type arg)
  | _ -> Location.raise_errorf ~loc "Unsupported type for patch derivation: %a" Pprintast.core_type ptyp

(** Generate the Patch module containing only the t type *)
let generate_patch_module ~ctxt:_ type_decl =
  let loc = type_decl.ptype_loc in
  match type_decl.ptype_kind with
  | Ptype_record fields ->
    (* Generate Patch.t record fields *)
    let patch_fields = List.map fields ~f:(fun (ld : label_declaration) ->
        { pld_name = ld.pld_name;
          pld_mutable = ld.pld_mutable;
          pld_type = classify_patch_type ld.pld_loc ld.pld_type;
          pld_attributes = [];
          pld_loc = ld.pld_loc; }
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
    let module_binding =
      { pmb_name = { txt = Some "Patch"; loc };
        pmb_expr = Ast_builder.Default.pmod_structure ~loc [patch_structure_item];
        pmb_attributes = [];
        pmb_loc = loc }
    in
    Ast_helper.Str.module_ ~loc module_binding
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
        let patch_fields = List.map fields ~f:(fun (ld : label_declaration) ->
            { pld_name = ld.pld_name;
              pld_mutable = ld.pld_mutable;
              pld_type = classify_patch_type ld.pld_loc ld.pld_type;
              pld_attributes = [];
              pld_loc = ld.pld_loc; }
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
        let module_sig =
          { pmd_name = { txt = Some "Patch"; loc };
            pmd_type = Ast_helper.Mty.signature ~loc [patch_sig_item];
            pmd_attributes = [];
            pmd_loc = loc }
        in
        [Ast_helper.Sig.module_ ~loc module_sig]
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
