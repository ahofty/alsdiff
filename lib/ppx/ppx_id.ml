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

(** Find all fields with [@id.id] attribute *)
let find_id_fields (fields : label_declaration list) : label_declaration list =
  List.filter fields ~f:field_has_id_attr

(** Extract field name as a string *)
let get_field_name (ld : label_declaration) : string =
  ld.pld_name.txt

(** Generate has_same_id function implementation *)
let generate_has_same_id_impl ~loc (_type_name : string) (fields : label_declaration list) : structure =
  let open Ast_builder.Default in
  match fields with
  | [] ->
    (* No ID fields - should not happen due to validation *)
    [Ast_builder.Default.pstr_extension ~loc
       (Location.error_extensionf ~loc "No [@id.id] attribute found") []]
  | [field] ->
    (* Single ID field - existing behavior *)
    let field_name = get_field_name field in
    let access_a = pexp_field ~loc (pexp_ident ~loc { txt = Lident "a"; loc }) { txt = Lident field_name; loc } in
    let access_b = pexp_field ~loc (pexp_ident ~loc { txt = Lident "b"; loc }) { txt = Lident field_name; loc } in
    [%str
      let has_same_id a b =
        [%e access_a] = [%e access_b]
    ]
  | fields ->
    (* Multiple ID fields - compound ID *)
    let comparisons = List.map fields ~f:(fun field ->
        let field_name = get_field_name field in
        let access_a = pexp_field ~loc (pexp_ident ~loc { txt = Lident "a"; loc }) { txt = Lident field_name; loc } in
        let access_b = pexp_field ~loc (pexp_ident ~loc { txt = Lident "b"; loc }) { txt = Lident field_name; loc } in
        [%expr [%e access_a] = [%e access_b]]
      ) in
    let body = Ppx_shared.chain_and ~loc comparisons in
    [%str
      let has_same_id (a : t) (b : t) =
        [%e body]
    ]

(** Generate id_hash function implementation *)
let generate_id_hash_impl ~loc (_type_name : string) (fields : label_declaration list) : structure =
  let open Ast_builder.Default in
  match fields with
  | [] ->
    (* No ID fields - should not happen due to validation *)
    [Ast_builder.Default.pstr_extension ~loc
       (Location.error_extensionf ~loc "No [@id.id] attribute found") []]
  | [field] ->
    (* Single ID field - existing behavior *)
    let field_name = get_field_name field in
    let access = pexp_field ~loc (pexp_ident ~loc { txt = Lident "t"; loc }) { txt = Lident field_name; loc } in
    [%str
      let id_hash t =
        Hashtbl.hash [%e access]
    ]
  | fields ->
    (* Multiple ID fields - hash tuple *)
    let field_accesses = List.map fields ~f:(fun field ->
        let field_name = get_field_name field in
        pexp_field ~loc (pexp_ident ~loc { txt = Lident "t"; loc }) { txt = Lident field_name; loc }
      ) in
    (* Build tuple expression: (t.field1, t.field2, ...) *)
    let tuple_expr = pexp_tuple ~loc field_accesses in
    [%str
      let id_hash (t : t) =
        Hashtbl.hash [%e tuple_expr]
    ]

(** Generate signature for has_same_id function *)
let generate_has_same_id_sig ~loc (_type_name : string) : signature =
  [%sig: val has_same_id : t -> t -> bool]

(** Generate signature for id_hash function *)
let generate_id_hash_sig ~loc (_type_name : string) : signature =
  [%sig: val id_hash : t -> int]

(** Shared validation and error handling for both impl and intf generation *)
let generate_with_validation
    ~loc
    (type_decl : type_declaration)
    (on_success : Location.t -> string -> label_declaration list -> 'a list)
    (mk_error_item : Location.t -> string -> 'a)
  : 'a list =
  match type_decl.ptype_kind with
  | Ptype_record fields ->
    let id_fields = find_id_fields fields in
    if List.is_empty id_fields then
      [mk_error_item type_decl.ptype_loc "Missing [@id.id] attribute on identifier field"]
    else
      let type_name = type_decl.ptype_name.txt in
      on_success loc type_name id_fields
  | _ ->
    [mk_error_item type_decl.ptype_loc "Cannot derive id for non-record types"]

(** Main implementation generator *)
let generate_impl ~ctxt (_rec_flag, type_declarations) : structure =
  let loc = Expansion_context.Deriver.derived_item_loc ctxt in
  List.map type_declarations ~f:(fun td ->
      generate_with_validation ~loc td
        (fun loc type_name fields ->
           generate_has_same_id_impl ~loc type_name fields
           @ generate_id_hash_impl ~loc type_name fields)
        (fun loc msg ->
           Ast_builder.Default.pstr_extension ~loc (Location.error_extensionf ~loc "%s" msg) [])
    )
  |> List.concat

(** Main signature generator *)
let generate_intf ~ctxt (_rec_flag, type_declarations) : signature =
  let loc = Expansion_context.Deriver.derived_item_loc ctxt in
  List.map type_declarations ~f:(fun td ->
      generate_with_validation ~loc td
        (fun loc type_name _fields ->
           generate_has_same_id_sig ~loc type_name
           @ generate_id_hash_sig ~loc type_name)
        (fun loc msg ->
           Ast_builder.Default.psig_extension ~loc (Location.error_extensionf ~loc "%s" msg) [])
    )
  |> List.concat

let impl_generator = Deriving.Generator.V2.make_noarg generate_impl
let intf_generator = Deriving.Generator.V2.make_noarg generate_intf

let deriver =
  Deriving.add "id" ~str_type_decl:impl_generator ~sig_type_decl:intf_generator
