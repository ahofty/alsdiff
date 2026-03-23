open Ppxlib
module List = ListLabels

(** Attribute name for marking the identifier field *)
let id_attr_name = "id.id"

(** Check if a field has the [@id.id] attribute *)
let field_has_id_attr (ld : label_declaration) : bool =
  List.exists ld.pld_attributes ~f:(fun attr ->
      let name = attr.attr_name.txt in
      String.equal name id_attr_name
    )

(** Find the field with [@id.id] attribute, returning None if not found or multiple found *)
let find_id_field (fields : label_declaration list) : label_declaration option =
  let id_fields = List.filter fields ~f:field_has_id_attr in
  match id_fields with
  | [] -> None
  | [field] -> Some field
  | _ -> None (* Multiple attributes found *)

(** Extract field name as a string *)
let get_field_name (ld : label_declaration) : string =
  ld.pld_name.txt

(** Generate has_same_id function implementation *)
let generate_has_same_id_impl ~loc (_type_name : string) (field_name : string) : structure =
  let open Ast_builder.Default in
  (* let has_same_id a b = a.field = b.field *)
  let pat_a = ppat_var ~loc { txt = "a"; loc } in
  let pat_b = ppat_var ~loc { txt = "b"; loc } in
  let access_a = pexp_field ~loc (pexp_ident ~loc { txt = Lident "a"; loc }) { txt = Lident field_name; loc } in
  let access_b = pexp_field ~loc (pexp_ident ~loc { txt = Lident "b"; loc }) { txt = Lident field_name; loc } in
  let body = pexp_apply ~loc (pexp_ident ~loc { txt = Lident "="; loc }) [Nolabel, access_a; Nolabel, access_b] in
  (* Use pexp_fun to create curried function: fun a b -> body *)
  let expr = pexp_fun ~loc Nolabel None pat_a (pexp_fun ~loc Nolabel None pat_b body) in
  let vb = value_binding ~loc ~pat:(ppat_var ~loc { txt = "has_same_id"; loc }) ~expr:expr in
  [pstr_value ~loc Nonrecursive [vb]]

(** Generate id_hash function implementation *)
let generate_id_hash_impl ~loc (_type_name : string) (field_name : string) : structure =
  let open Ast_builder.Default in
  (* let id_hash t = Hashtbl.hash t.field *)
  let pat_t = ppat_var ~loc { txt = "t"; loc } in
  let access = pexp_field ~loc (pexp_ident ~loc { txt = Lident "t"; loc }) { txt = Lident field_name; loc } in
  let hashtbl_hash = pexp_ident ~loc { txt = Ldot (Lident "Hashtbl", "hash"); loc } in
  let body = pexp_apply ~loc hashtbl_hash [Nolabel, access] in
  (* Use pexp_fun to create curried function: fun t -> body *)
  let expr = pexp_fun ~loc Nolabel None pat_t body in
  let vb = value_binding ~loc ~pat:(ppat_var ~loc { txt = "id_hash"; loc }) ~expr:expr in
  [pstr_value ~loc Nonrecursive [vb]]

(** Generate signature for has_same_id function *)
let generate_has_same_id_sig ~loc (_type_name : string) (_field_name : string) : signature =
  let open Ast_builder.Default in
  let has_same_id_sig =
    value_description
      ~loc
      ~name:{ txt = "has_same_id"; loc }
      ~type_:(ptyp_arrow ~loc Nolabel (ptyp_var ~loc "t")
                (ptyp_arrow ~loc Nolabel (ptyp_var ~loc "t")
                   (ptyp_constr ~loc { txt = Lident "bool"; loc } [])))
      ~prim:[]
  in
  [psig_value ~loc has_same_id_sig]

(** Generate signature for id_hash function *)
let generate_id_hash_sig ~loc (_type_name : string) (_field_name : string) : signature =
  let open Ast_builder.Default in
  let id_hash_sig =
    value_description
      ~loc
      ~name:{ txt = "id_hash"; loc }
      ~type_:(ptyp_arrow ~loc Nolabel (ptyp_var ~loc "t")
                (ptyp_constr ~loc { txt = Lident "int"; loc } []))
      ~prim:[]
  in
  [psig_value ~loc id_hash_sig]

(** Main implementation generator *)
let generate_impl ~ctxt (_rec_flag, type_declarations) : structure =
  let loc = Expansion_context.Deriver.derived_item_loc ctxt in
  List.map type_declarations ~f:(fun td ->
      match td.ptype_kind with
      | Ptype_record fields ->
        (match find_id_field fields with
         | Some id_field ->
           let field_name = get_field_name id_field in
           let type_name = td.ptype_name.txt in
           generate_has_same_id_impl ~loc type_name field_name
           @ generate_id_hash_impl ~loc type_name field_name
         | None ->
           let id_fields = List.filter fields ~f:field_has_id_attr in
           if List.is_empty id_fields then
             let ext = Location.error_extensionf ~loc:td.ptype_loc
                 "Missing [@id.id] attribute on identifier field" in
             [Ast_helper.Str.extension ~loc ~attrs:[] ext]
           else
             let ext = Location.error_extensionf ~loc:td.ptype_loc
                 "Multiple [@id.id] attributes found, only one is allowed" in
             [Ast_helper.Str.extension ~loc ~attrs:[] ext])
      | _ ->
        let ext = Location.error_extensionf ~loc:td.ptype_loc
            "Cannot derive id for non-record types" in
        [Ast_helper.Str.extension ~loc ~attrs:[] ext]
    )
  |> List.concat

(** Main signature generator *)
let generate_intf ~ctxt (_rec_flag, type_declarations) : signature =
  let loc = Expansion_context.Deriver.derived_item_loc ctxt in
  List.map type_declarations ~f:(fun td ->
      match td.ptype_kind with
      | Ptype_record fields ->
        (match find_id_field fields with
         | Some id_field ->
           let field_name = get_field_name id_field in
           let type_name = td.ptype_name.txt in
           generate_has_same_id_sig ~loc type_name field_name
           @ generate_id_hash_sig ~loc type_name field_name
         | None ->
           let id_fields = List.filter fields ~f:field_has_id_attr in
           if List.is_empty id_fields then
             let ext = Location.error_extensionf ~loc:td.ptype_loc
                 "Missing [@id.id] attribute on identifier field" in
             [Ast_helper.Sig.extension ~loc ~attrs:[] ext]
           else
             let ext = Location.error_extensionf ~loc:td.ptype_loc
                 "Multiple [@id.id] attributes found, only one is allowed" in
             [Ast_helper.Sig.extension ~loc ~attrs:[] ext])
      | _ ->
        let ext = Location.error_extensionf ~loc:td.ptype_loc
            "Cannot derive id for non-record types" in
        [Ast_helper.Sig.extension ~loc ~attrs:[] ext]
    )
  |> List.concat

let impl_generator = Deriving.Generator.V2.make_noarg generate_impl
let intf_generator = Deriving.Generator.V2.make_noarg generate_intf

let deriver =
  Deriving.add "id" ~str_type_decl:impl_generator ~sig_type_decl:intf_generator
