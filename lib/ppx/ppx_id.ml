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
  (* Build field access expressions: a.field and b.field *)
  let access_a = pexp_field ~loc (pexp_ident ~loc { txt = Lident "a"; loc }) { txt = Lident field_name; loc } in
  let access_b = pexp_field ~loc (pexp_ident ~loc { txt = Lident "b"; loc }) { txt = Lident field_name; loc } in
  (* Use metaquot for readable function definition *)
  [%str
    let has_same_id a b =
      [%e access_a] = [%e access_b]
  ]

(** Generate id_hash function implementation *)
let generate_id_hash_impl ~loc (_type_name : string) (field_name : string) : structure =
  let open Ast_builder.Default in
  (* Build field access expression: t.field *)
  let access = pexp_field ~loc (pexp_ident ~loc { txt = Lident "t"; loc }) { txt = Lident field_name; loc } in
  (* Use metaquot for readable function definition *)
  [%str
    let id_hash t =
      Hashtbl.hash [%e access]
  ]

(** Generate signature for has_same_id function *)
let generate_has_same_id_sig ~loc (_type_name : string) (_field_name : string) : signature =
  [%sig: val has_same_id : t -> t -> bool]

(** Generate signature for id_hash function *)
let generate_id_hash_sig ~loc (_type_name : string) (_field_name : string) : signature =
  [%sig: val id_hash : t -> int]

(** Shared validation and error handling for both impl and intf generation *)
let generate_with_validation
    ~loc
    (type_decl : type_declaration)
    (on_success : Location.t -> string -> string -> 'a list)
    (mk_error_item : Location.t -> string -> 'a)
  : 'a list =
  match type_decl.ptype_kind with
  | Ptype_record fields ->
    (match find_id_field fields with
     | Some id_field ->
       let field_name = get_field_name id_field in
       let type_name = type_decl.ptype_name.txt in
       on_success loc type_name field_name
     | None ->
       let id_fields = List.filter fields ~f:field_has_id_attr in
       if List.is_empty id_fields then
         [mk_error_item type_decl.ptype_loc "Missing [@id.id] attribute on identifier field"]
       else
         [mk_error_item type_decl.ptype_loc "Multiple [@id.id] attributes found, only one is allowed"])
  | _ ->
    [mk_error_item type_decl.ptype_loc "Cannot derive id for non-record types"]

(** Main implementation generator *)
let generate_impl ~ctxt (_rec_flag, type_declarations) : structure =
  let loc = Expansion_context.Deriver.derived_item_loc ctxt in
  List.map type_declarations ~f:(fun td ->
      generate_with_validation ~loc td
        (fun loc type_name field_name ->
           generate_has_same_id_impl ~loc type_name field_name
           @ generate_id_hash_impl ~loc type_name field_name)
        (fun loc msg ->
           Ast_builder.Default.pstr_extension ~loc (Location.error_extensionf ~loc "%s" msg) [])
    )
  |> List.concat

(** Main signature generator *)
let generate_intf ~ctxt (_rec_flag, type_declarations) : signature =
  let loc = Expansion_context.Deriver.derived_item_loc ctxt in
  List.map type_declarations ~f:(fun td ->
      generate_with_validation ~loc td
        (fun loc type_name field_name ->
           generate_has_same_id_sig ~loc type_name field_name
           @ generate_id_hash_sig ~loc type_name field_name)
        (fun loc msg ->
           Ast_builder.Default.psig_extension ~loc (Location.error_extensionf ~loc "%s" msg) [])
    )
  |> List.concat

let impl_generator = Deriving.Generator.V2.make_noarg generate_impl
let intf_generator = Deriving.Generator.V2.make_noarg generate_intf

let deriver =
  Deriving.add "id" ~str_type_decl:impl_generator ~sig_type_decl:intf_generator
