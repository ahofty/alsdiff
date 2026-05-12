open Ppxlib
module List = ListLabels

(* Flatten a Longident.t to a string list *)
let rec lid_to_list : Longident.t -> string list = function
  | Lident s -> [s]
  | Ldot (lid, s) -> lid_to_list lid @ [s]
  | Lapply _ -> []

(* ==================== Attribute helpers ==================== *)

let has_attribute (attrs : attributes) attr_name =
  List.exists attrs ~f:(fun attr ->
      String.equal attr.attr_name.txt attr_name)

(* Extract a string from an attribute payload like [@view.label "X"] *)
let extract_string_payload (payload : payload) : string option =
  match payload with
  | PStr [{ pstr_desc = Pstr_eval ({ pexp_desc = Pexp_constant (Pconst_string (s, _, _)); _ }, _); _ }] ->
    Some s
  | _ -> None

(* Extract an identifier from an attribute payload like [@view.scalar time] *)
let extract_ident_payload (payload : payload) : string option =
  match payload with
  | PStr [{ pstr_desc = Pstr_eval ({ pexp_desc = Pexp_ident { txt = Lident name; _ }; _ }, _); _ }] ->
    Some name
  | _ -> None

(* ==================== View attribute parsing ==================== *)

let get_scalar_kind (attrs : attributes) : string option =
  List.find_map attrs ~f:(fun attr ->
      if String.equal attr.attr_name.txt "view.scalar"
      then extract_ident_payload attr.attr_payload
      else None)

let is_const_attr (attrs : attributes) : bool =
  has_attribute attrs "view.const"

let get_custom_fn (attrs : attributes) : string option =
  List.find_map attrs ~f:(fun attr ->
      if String.equal attr.attr_name.txt "view.custom"
      then extract_ident_payload attr.attr_payload
      else None)

let has_skip_attr (attrs : attributes) : bool =
  has_attribute attrs "view.skip"

let get_label (attrs : attributes) : string option =
  List.find_map attrs ~f:(fun attr ->
      if String.equal attr.attr_name.txt "view.label"
      then extract_string_payload attr.attr_payload
      else None)

let get_child_domain (attrs : attributes) : string option =
  List.find_map attrs ~f:(fun attr ->
      if String.equal attr.attr_name.txt "view.child"
      then extract_string_payload attr.attr_payload
      else None)

let get_optional_child_domain (attrs : attributes) : string option =
  List.find_map attrs ~f:(fun attr ->
      if String.equal attr.attr_name.txt "view.optional_child"
      then extract_string_payload attr.attr_payload
      else None)

let get_collection_domain (attrs : attributes) : string option =
  List.find_map attrs ~f:(fun attr ->
      if String.equal attr.attr_name.txt "view.collection"
      then extract_string_payload attr.attr_payload
      else None)

let has_inline_child_attr (attrs : attributes) : bool =
  has_attribute attrs "view.inline_child"

let has_name_attr (attrs : attributes) : bool =
  has_attribute attrs "view.name"

let has_name_patch_attr (attrs : attributes) : bool =
  has_attribute attrs "view.name_patch"

let has_display_attr (attrs : attributes) : bool =
  has_attribute attrs "view.display"

let get_type_label (attrs : attributes) : string option =
  List.find_map attrs ~f:(fun attr ->
      if String.equal attr.attr_name.txt "view.type_label"
      then extract_string_payload attr.attr_payload
      else None)

let get_builder_name (attrs : attributes) : string option =
  List.find_map attrs ~f:(fun attr ->
      if String.equal attr.attr_name.txt "view.builder"
      then extract_string_payload attr.attr_payload
      else None)

(* ==================== Label generation ==================== *)

(* Convert snake_case to title case: start_time -> "Start Time", on -> "On" *)
let field_name_to_label (name : string) : string =
  let buf = Buffer.create (String.length name) in
  String.iteri (fun i c ->
      if c = '_' then Buffer.add_char buf ' '
      else if i = 0 || (i > 0 && name.[i - 1] = '_') then Buffer.add_char buf (Char.uppercase_ascii c)
      else Buffer.add_char buf c
    ) name;
  Buffer.contents buf

let get_field_label (field_name : string) (attrs : attributes) : string =
  match get_label attrs with
  | Some s -> s
  | None -> field_name_to_label field_name

(* ==================== Type classification ==================== *)

let is_atomic_type_name : core_type -> string option = function
  | { ptyp_desc = Ptyp_constr ({ txt = Lident "int"; _ }, []); _ } -> Some "int"
  | { ptyp_desc = Ptyp_constr ({ txt = Lident "float"; _ }, []); _ } -> Some "float"
  | { ptyp_desc = Ptyp_constr ({ txt = Lident "string"; _ }, []); _ } -> Some "string"
  | { ptyp_desc = Ptyp_constr ({ txt = Lident "bool"; _ }, []); _ } -> Some "bool"
  | _ -> None

let make_maker_name = function
  | "int" -> "B.make_int" | "float" -> "B.make_float"
  | "string" -> "B.make_string" | "bool" -> "B.make_bool"
  | _ -> assert false

let make_wrapper_name = function
  | "int" -> "B.int_value" | "float" -> "B.float_value"
  | "string" -> "B.string_value" | "bool" -> "B.bool_value"
  | _ -> assert false

(* Get the module path from a type like Loop.t -> Longident for "Loop" *)
let module_path_of_type (ptyp : core_type) : Longident.t option =
  match ptyp.ptyp_desc with
  | Ptyp_constr ({ txt = Ldot (prefix, "t"); _ }, []) -> Some prefix
  | _ -> None

let module_path_of_option_type (ptyp : core_type) : Longident.t option =
  match ptyp.ptyp_desc with
  | Ptyp_constr ({ txt = Lident "option"; _ }, [inner]) -> module_path_of_type inner
  | _ -> None

let module_path_of_list_type (ptyp : core_type) : Longident.t option =
  match ptyp.ptyp_desc with
  | Ptyp_constr ({ txt = Lident "list"; _ }, [inner]) -> module_path_of_type inner
  | _ -> None

(* ==================== Code generation helpers ==================== *)

let mk_lid_expr loc lid =
  Ast_builder.Default.pexp_ident ~loc { loc; txt = lid }

let mk_str loc s = Ast_builder.Default.estring ~loc s

let generate_value_accessor ~loc field_name =
  let open Ast_builder.Default in
  pexp_fun ~loc Nolabel None
    (ppat_var ~loc { txt = "v"; loc })
    (pexp_field ~loc
       (pexp_ident ~loc { txt = Lident "v"; loc })
       { loc; txt = Lident field_name })

let generate_patch_accessor ~loc field_name =
  let open Ast_builder.Default in
  pexp_fun ~loc Nolabel None
    (ppat_var ~loc { txt = "p"; loc })
    (pexp_field ~loc
       (pexp_ident ~loc { txt = Lident "p"; loc })
       { loc; txt = Ldot (Lident "Patch", field_name) })

(* Build a list expression from a list of expressions *)
let mk_list_expr loc exprs =
  let open Ast_builder.Default in
  List.fold_right exprs ~init:(pexp_construct ~loc { txt = Lident "[]"; loc } None)
    ~f:(fun e acc -> pexp_construct ~loc { txt = Lident "::"; loc } (Some (pexp_tuple ~loc [e; acc])))

(* ==================== Field spec generators ==================== *)

let generate_atomic_field_spec ~loc label field_name maker =
  let open Ast_builder.Default in
  pexp_apply ~loc (mk_lid_expr loc (Longident.parse maker))
    [ Nolabel, mk_str loc label
    ; Nolabel, generate_value_accessor ~loc field_name
    ; Nolabel, generate_patch_accessor ~loc field_name ]

let generate_time_field_spec ~loc label field_name =
  let open Ast_builder.Default in
  pexp_apply ~loc (mk_lid_expr loc (Ldot (Lident "B", "make_time_field")))
    [ Nolabel, pexp_ident ~loc { txt = Lident "format_time"; loc }
    ; Nolabel, mk_str loc label
    ; Nolabel, generate_value_accessor ~loc field_name
    ; Nolabel, generate_patch_accessor ~loc field_name ]

let generate_unix_timestamp_field_spec ~loc label field_name =
  let open Ast_builder.Default in
  let wrapper =
    pexp_fun ~loc Nolabel None
      (ppat_var ~loc { txt = "x"; loc })
      (pexp_apply ~loc (mk_lid_expr loc (Ldot (Lident "B", "string_value")))
         [ Nolabel, pexp_apply ~loc (mk_lid_expr loc (Ldot (Lident "B", "format_unix_timestamp")))
             [ Nolabel, pexp_ident ~loc { txt = Lident "x"; loc } ] ])
  in
  pexp_apply ~loc (mk_lid_expr loc (Ldot (Lident "B", "make_spec")))
    [ Nolabel, wrapper
    ; Nolabel, mk_str loc label
    ; Nolabel, generate_value_accessor ~loc field_name
    ; Nolabel, generate_patch_accessor ~loc field_name ]

let generate_const_field_spec ~loc label field_name atomic_type =
  let open Ast_builder.Default in
  pexp_apply ~loc (mk_lid_expr loc (Ldot (Lident "B", "make_spec_const")))
    [ Nolabel, mk_lid_expr loc (Longident.parse (make_wrapper_name atomic_type))
    ; Nolabel, mk_str loc label
    ; Nolabel, generate_value_accessor ~loc field_name ]

let generate_custom_field_spec ~loc label field_name custom_fn =
  let open Ast_builder.Default in
  pexp_apply ~loc (mk_lid_expr loc (Ldot (Lident "B", "make_spec")))
    [ Nolabel, mk_lid_expr loc (Longident.parse custom_fn)
    ; Nolabel, mk_str loc label
    ; Nolabel, generate_value_accessor ~loc field_name
    ; Nolabel, generate_patch_accessor ~loc field_name ]

(* ==================== Section spec generators ==================== *)

let lid_of_strings = function
  | [] -> assert false
  | [x] -> Lident x
  | x :: xs -> List.fold_left xs ~init:(Lident x) ~f:(fun acc s -> Ldot (acc, s))

let mk_vs_field_access loc view_spec_lid field_name arg_var =
  let open Ast_builder.Default in
  let vs_mod =
    pmod_apply ~loc
      (pmod_ident ~loc { loc; txt = view_spec_lid })
      (pmod_ident ~loc { loc; txt = Lident "B" })
  in
  pexp_fun ~loc Nolabel None
    (ppat_var ~loc { txt = arg_var; loc })
    (pexp_letmodule ~loc { txt = Some "Vs"; loc } vs_mod
       (pexp_apply ~loc
          (pexp_ident ~loc { loc; txt = Ldot (Lident "Vs", field_name) })
          [ Nolabel, pexp_ident ~loc { txt = Lident arg_var; loc } ]))

let generate_child_spec ~loc field_name label domain_type_name child_mod_lid =
  let open Ast_builder.Default in
  let mod_path = lid_to_list child_mod_lid in
  let view_spec_lid = lid_of_strings (mod_path @ ["ViewSpec"]) in
  let build_value_fn = mk_vs_field_access loc view_spec_lid "build_value_fields" "ct" in
  let build_patch_fn = mk_vs_field_access loc view_spec_lid "build_patch_fields" "np" in
  pexp_apply ~loc (mk_lid_expr loc (Ldot (Ldot (Lident "B", "Spec"), "child")))
    [ Labelled "name", mk_str loc label
    ; Labelled "of_value", generate_value_accessor ~loc field_name
    ; Labelled "of_patch", generate_patch_accessor ~loc field_name
    ; Labelled "build_value_children", build_value_fn
    ; Labelled "build_patch_children", build_patch_fn
    ; Labelled "domain_type", pexp_apply ~loc (mk_lid_expr loc (Ldot (Lident "B", "domain_type_of_name")))
        [ Nolabel, mk_str loc domain_type_name ] ]

let generate_optional_child_spec ~loc field_name label domain_type_name child_mod_lid =
  let open Ast_builder.Default in
  let mod_path = lid_to_list child_mod_lid in
  let view_spec_lid = lid_of_strings (mod_path @ ["ViewSpec"]) in
  let build_value_fn = mk_vs_field_access loc view_spec_lid "build_value_fields" "ct" in
  let build_patch_fn = mk_vs_field_access loc view_spec_lid "build_patch_fields" "np" in
  pexp_apply ~loc (mk_lid_expr loc (Ldot (Ldot (Lident "B", "Spec"), "child_optional")))
    [ Labelled "name", mk_str loc label
    ; Labelled "of_value", generate_value_accessor ~loc field_name
    ; Labelled "of_patch", generate_patch_accessor ~loc field_name
    ; Labelled "build_value_children", build_value_fn
    ; Labelled "build_patch_children", build_patch_fn
    ; Labelled "domain_type", pexp_apply ~loc (mk_lid_expr loc (Ldot (Lident "B", "domain_type_of_name")))
        [ Nolabel, mk_str loc domain_type_name ] ]

let generate_collection_spec ~loc field_name label domain_type_name item_mod_lid =
  let open Ast_builder.Default in
  let mod_path = lid_to_list item_mod_lid in
  let view_spec_lid = lid_of_strings (mod_path @ ["ViewSpec"]) in
  let build_item_fn = mk_vs_field_access loc view_spec_lid "build_item" "ic" in
  pexp_apply ~loc (mk_lid_expr loc (Ldot (Ldot (Lident "B", "Spec"), "collection")))
    [ Labelled "name", mk_str loc label
    ; Labelled "of_value", generate_value_accessor ~loc field_name
    ; Labelled "of_patch", generate_patch_accessor ~loc field_name
    ; Labelled "build_item", build_item_fn
    ; Labelled "domain_type", pexp_apply ~loc (mk_lid_expr loc (Ldot (Lident "B", "domain_type_of_name")))
        [ Nolabel, mk_str loc domain_type_name ] ]

let generate_inline_child_binding ~loc field_name child_mod_lid =
  let open Ast_builder.Default in
  let mod_path = lid_to_list child_mod_lid in
  let view_spec_lid = lid_of_strings (mod_path @ ["ViewSpec"]) in
  let binding_name = "__inline_" ^ field_name in
  let vs_mod =
    pmod_apply ~loc
      (pmod_ident ~loc { loc; txt = view_spec_lid })
      (pmod_ident ~loc { loc; txt = Lident "B" })
  in
  let v_pat = ppat_var ~loc { txt = "v"; loc } in
  let p_pat = ppat_var ~loc { txt = "p"; loc } in
  let f_v =
    pexp_fun ~loc Nolabel None v_pat
      (pexp_field ~loc
         (pexp_ident ~loc { txt = Lident "v"; loc })
         { loc; txt = Lident field_name })
  in
  let f_p =
    pexp_fun ~loc Nolabel None p_pat
      (pexp_field ~loc
         (pexp_ident ~loc { txt = Lident "p"; loc })
         { loc; txt = Ldot (Lident "Patch", field_name) })
  in
  let map_call =
    pexp_apply ~loc (mk_lid_expr loc (Ldot (Lident "B", "map_specs")))
      [ Nolabel, f_v
      ; Nolabel, f_p
      ; Nolabel, pexp_ident ~loc { loc; txt = Ldot (Lident "Vs", "field_specs") } ]
  in
  let body = pexp_letmodule ~loc { txt = Some "Vs"; loc } vs_mod map_call in
  pstr_value ~loc Nonrecursive [{
      pvb_pat = ppat_var ~loc { txt = binding_name; loc };
      pvb_expr = body;
      pvb_attributes = [];
      pvb_loc = loc;
      pvb_constraint = None }]

(* ==================== Field classification ==================== *)

type field_class =
  | Inline_atomic of string
  | Inline_time
  | Inline_unix_timestamp
  | Inline_const of string
  | Inline_custom of string
  | Inline_child of Longident.t
  | Nested_child of string * Longident.t
  | Nested_optional_child of string * Longident.t
  | Nested_collection of string * Longident.t
  | Skipped

let classify_field (ld : label_declaration) : field_class =
  let attrs = ld.pld_attributes in
  if has_skip_attr attrs || has_name_patch_attr attrs || has_display_attr attrs then Skipped
  else if has_attribute attrs "patch.skip"
       && not (has_attribute attrs "view.child"
               || has_attribute attrs "view.optional_child"
               || has_attribute attrs "view.collection"
               || has_attribute attrs "view.const"
               || has_inline_child_attr attrs
               || Option.is_some (get_scalar_kind attrs)
               || Option.is_some (get_custom_fn attrs)
               || Option.is_some (get_label attrs))
  then Skipped
  else
    match get_child_domain attrs with
    | Some dt ->
      (match module_path_of_type ld.pld_type with
       | Some mp -> Nested_child (dt, mp)
       | None -> Skipped)
    | None ->
      (match get_optional_child_domain attrs with
       | Some dt ->
         (match module_path_of_option_type ld.pld_type with
          | Some mp -> Nested_optional_child (dt, mp)
          | None -> Skipped)
       | None ->
         (match get_collection_domain attrs with
          | Some dt ->
            (match module_path_of_list_type ld.pld_type with
             | Some mp -> Nested_collection (dt, mp)
             | None -> Skipped)
          | None ->
            if has_inline_child_attr attrs then
              (match module_path_of_type ld.pld_type with
               | Some mp -> Inline_child mp
               | None -> Skipped)
            else
              (match get_scalar_kind attrs with
               | Some "time" -> Inline_time
               | Some "unix_timestamp" -> Inline_unix_timestamp
               | _ ->
                 if is_const_attr attrs then
                   (match is_atomic_type_name ld.pld_type with
                    | Some at -> Inline_const at
                    | None -> Skipped)
                 else
                   (match get_custom_fn attrs with
                    | Some fn -> Inline_custom fn
                    | None ->
                      (match is_atomic_type_name ld.pld_type with
                       | Some at -> Inline_atomic at
                       | None -> Skipped)))))

(* ==================== Naming info extraction ==================== *)

type naming_info = {
  name_field : string option;
  name_patch_field : string option;
  display_field : string option;
  type_label : string option;
  id_field : string option;
}

let extract_naming_info (type_decl : type_declaration) (fields : label_declaration list) : naming_info = {
  name_field = List.find_map fields ~f:(fun ld ->
      if has_name_attr ld.pld_attributes then Some ld.pld_name.txt else None);
  name_patch_field = List.find_map fields ~f:(fun ld ->
      if has_name_patch_attr ld.pld_attributes then Some ld.pld_name.txt else None);
  display_field = List.find_map fields ~f:(fun ld ->
      if has_display_attr ld.pld_attributes then Some ld.pld_name.txt else None);
  type_label = get_type_label type_decl.ptype_attributes;
  id_field = List.find_map fields ~f:(fun ld ->
      if has_attribute ld.pld_attributes "id.id" then Some ld.pld_name.txt else None);
}

(* ==================== build_section_name generation ==================== *)

let generate_build_section_name ~loc (ni : naming_info) =
  let open Ast_builder.Default in
  let label_val = match ni.type_label with Some s -> s | None -> "" in
  let c_var = pexp_ident ~loc { txt = Lident "c"; loc } in
  let v_var = pexp_ident ~loc { txt = Lident "v"; loc } in
  let p_var = pexp_ident ~loc { txt = Lident "p"; loc } in
  let tl_var = pexp_ident ~loc { txt = Lident "type_label"; loc } in

  let sprintf_expr args =
    pexp_apply ~loc (mk_lid_expr loc (Ldot (Lident "Printf", "sprintf"))) args
  in

  (* `Added v | `Removed v -> Printf.sprintf ... *)
  let added_removed_rhs =
    let name_f = Option.get ni.name_field in
    match ni.id_field with
    | Some id_f ->
      sprintf_expr [
        Nolabel, mk_str loc "%s (#%d): %s";
        Nolabel, tl_var;
        Nolabel, pexp_field ~loc v_var { loc; txt = Lident id_f };
        Nolabel, pexp_field ~loc v_var { loc; txt = Lident name_f };
      ]
    | None ->
      sprintf_expr [
        Nolabel, mk_str loc "%s: %s";
        Nolabel, tl_var;
        Nolabel, pexp_field ~loc v_var { loc; txt = Lident name_f };
      ]
  in

  (* `Modified p -> Printf.sprintf ... *)
  let modified_rhs =
    let np_f = Option.get ni.name_patch_field in
    match ni.id_field with
    | Some id_f ->
      sprintf_expr [
        Nolabel, mk_str loc "%s (#%d): %s";
        Nolabel, tl_var;
        Nolabel, pexp_field ~loc p_var { loc; txt = Lident id_f };
        Nolabel, pexp_field ~loc p_var { loc; txt = Lident np_f };
      ]
    | None ->
      sprintf_expr [
        Nolabel, mk_str loc "%s: %s";
        Nolabel, tl_var;
        Nolabel, pexp_field ~loc p_var { loc; txt = Lident np_f };
      ]
  in

  let cases = [
    { Parsetree.pc_lhs = ppat_or ~loc
          (ppat_variant ~loc "Added" (Some (ppat_var ~loc { txt = "v"; loc })))
          (ppat_variant ~loc "Removed" (Some (ppat_var ~loc { txt = "v"; loc })));
      pc_guard = None;
      pc_rhs = added_removed_rhs };
    { Parsetree.pc_lhs = ppat_variant ~loc "Modified" (Some (ppat_var ~loc { txt = "p"; loc }));
      pc_guard = None;
      pc_rhs = modified_rhs };
    { Parsetree.pc_lhs = ppat_variant ~loc "Unchanged" None;
      pc_guard = None;
      pc_rhs = tl_var };
  ] in

  let match_body = pexp_match ~loc c_var cases in
  let c_pat =
    let open Ast_builder.Default in
    ppat_constraint ~loc
      (ppat_var ~loc { txt = "c"; loc })
      (ptyp_constr ~loc
         { loc; txt = Ldot (Ldot (Lident "Alsdiff_base", "Diff"), "structured_change") }
         [ ptyp_constr ~loc { loc; txt = Lident "t" } []
         ; ptyp_constr ~loc { loc; txt = Ldot (Lident "Patch", "t") } [] ])
  in
  let with_c =
    pexp_fun ~loc Nolabel None c_pat match_body
  in
  let with_type_label =
    pexp_fun ~loc (Optional "type_label")
      (Some (mk_str loc label_val))
      (ppat_var ~loc { txt = "type_label"; loc })
      with_c
  in
  pstr_value ~loc Nonrecursive [{
      pvb_pat = ppat_var ~loc { txt = "build_section_name"; loc };
      pvb_expr = with_type_label;
      pvb_attributes = [];
      pvb_loc = loc;
      pvb_constraint = None }]

(* ==================== Builder collection spec generator ==================== *)

let generate_builder_collection_spec ~loc field_name label domain_type_name builder_name =
  let open Ast_builder.Default in
  pexp_apply ~loc (mk_lid_expr loc (Ldot (Ldot (Lident "B", "Spec"), "collection")))
    [ Labelled "name", mk_str loc label
    ; Labelled "of_value", generate_value_accessor ~loc field_name
    ; Labelled "of_patch", generate_patch_accessor ~loc field_name
    ; Labelled "build_item", pexp_ident ~loc { txt = Lident builder_name; loc }
    ; Labelled "domain_type", pexp_apply ~loc (mk_lid_expr loc (Ldot (Lident "B", "domain_type_of_name")))
        [ Nolabel, mk_str loc domain_type_name ] ]

(* ==================== Main code generation ==================== *)

let generate_specs_from_fields ~loc fields =
  let has_time_field = List.exists fields ~f:(fun ld ->
      match classify_field ld with Inline_time -> true | _ -> false) in
  let field_specs = List.filter_map fields ~f:(fun ld ->
      let fname = ld.pld_name.txt in
      let label = get_field_label fname ld.pld_attributes in
      match classify_field ld with
      | Inline_atomic at -> Some (generate_atomic_field_spec ~loc label fname (make_maker_name at))
      | Inline_time -> Some (generate_time_field_spec ~loc label fname)
      | Inline_unix_timestamp -> Some (generate_unix_timestamp_field_spec ~loc label fname)
      | Inline_const at -> Some (generate_const_field_spec ~loc label fname at)
      | Inline_custom fn -> Some (generate_custom_field_spec ~loc label fname fn)
      | _ -> None) in
  let builder_fields = List.filter_map fields ~f:(fun ld ->
      match (classify_field ld, get_builder_name ld.pld_attributes) with
      | Nested_collection _, Some bname -> Some (ld.pld_name.txt, bname)
      | _ -> None) in
  let child_section_specs = List.filter_map fields ~f:(fun ld ->
      let fname = ld.pld_name.txt in
      let label = get_field_label fname ld.pld_attributes in
      match classify_field ld with
      | Nested_child (dt, mp) -> Some (generate_child_spec ~loc fname label dt mp)
      | Nested_optional_child (dt, mp) -> Some (generate_optional_child_spec ~loc fname label dt mp)
      | Nested_collection (dt, mp) ->
        (match get_builder_name ld.pld_attributes with
         | Some bname -> Some (generate_builder_collection_spec ~loc fname label dt bname)
         | None -> Some (generate_collection_spec ~loc fname label dt mp))
      | _ -> None) in
  let inline_child_fields = List.filter_map fields ~f:(fun ld ->
      match classify_field ld with
      | Inline_child mp -> Some (ld.pld_name.txt, mp)
      | _ -> None) in
  (has_time_field, field_specs, child_section_specs, inline_child_fields, builder_fields)

let generate_view_spec_impl ~ctxt:_ (_rec_flag, type_decls) =
  match type_decls with
  | [] -> []
  | type_decl :: _ ->
    let open Ast_builder.Default in
    let loc = type_decl.ptype_loc in
    let fields = match type_decl.ptype_kind with
      | Ptype_record fields -> fields
      | _ -> []
    in
    let (has_time_field, field_specs_exprs, child_section_specs, inline_child_fields, builder_fields) =
      generate_specs_from_fields ~loc fields
    in
    let ni = extract_naming_info type_decl fields in
    let has_naming = ni.name_field <> None && ni.name_patch_field <> None && ni.type_label <> None in

    (* --- inline_child bindings --- *)
    let inline_bindings = List.map inline_child_fields ~f:(fun (fname, mp) ->
        generate_inline_child_binding ~loc fname mp) in

    (* --- field_specs binding --- *)
    let field_specs_base = mk_list_expr loc field_specs_exprs in
    let field_specs_list =
      List.fold_left inline_child_fields ~init:field_specs_base
        ~f:(fun acc (fname, _) ->
            let inline_ref = pexp_ident ~loc { txt = Lident ("__inline_" ^ fname); loc } in
            pexp_apply ~loc
              (pexp_ident ~loc { loc; txt = Ldot (Lident "List", "append") })
              [ Nolabel, acc; Nolabel, inline_ref ])
    in
    let field_specs_binding =
      let expr =
        if has_time_field then
          pexp_fun ~loc (Labelled "format_time")
            None
            (ppat_var ~loc { txt = "format_time"; loc })
            field_specs_list
        else field_specs_list
      in
      pstr_value ~loc Nonrecursive [{
          pvb_pat = ppat_var ~loc { txt = "field_specs"; loc };
          pvb_expr = expr;
          pvb_attributes = [];
          pvb_loc = loc;
          pvb_constraint = None }]
    in

    (* --- section_specs binding --- *)
    let specs_arg =
      if has_time_field then
        pexp_apply ~loc (pexp_ident ~loc { txt = Lident "field_specs"; loc })
          [Labelled "format_time", pexp_ident ~loc { txt = Lident "format_time"; loc }]
      else
        pexp_ident ~loc { txt = Lident "field_specs"; loc }
    in
    let inline_section =
      pexp_apply ~loc (mk_lid_expr loc (Ldot (Ldot (Lident "B", "Spec"), "inline_fields")))
        [ Labelled "specs", specs_arg
        ; Labelled "domain_type", pexp_apply ~loc
            (mk_lid_expr loc (Ldot (Lident "B", "domain_type_of_name")))
            [Nolabel, mk_str loc "DTDefault"] ]
    in
    let all_sections = inline_section :: child_section_specs in
    let section_specs_list = mk_list_expr loc all_sections in
    let section_specs_binding =
      let expr =
        let base =
          if has_time_field then
            pexp_fun ~loc (Labelled "format_time")
              None
              (ppat_var ~loc { txt = "format_time"; loc })
              section_specs_list
          else section_specs_list
        in
        List.fold_left builder_fields ~init:base
          ~f:(fun acc (_, bname) ->
              pexp_fun ~loc (Labelled bname)
                None
                (ppat_var ~loc { txt = bname; loc })
                acc)
      in
      pstr_value ~loc Nonrecursive [{
          pvb_pat = ppat_var ~loc { txt = "section_specs"; loc };
          pvb_expr = expr;
          pvb_attributes = [];
          pvb_loc = loc;
          pvb_constraint = None }]
    in

    (* --- build_value_fields binding --- *)
    let build_value_fields_binding =
      let fs_call =
        if has_time_field then
          pexp_apply ~loc (pexp_ident ~loc { txt = Lident "field_specs"; loc })
            [Labelled "format_time", pexp_ident ~loc { txt = Lident "format_time"; loc }]
        else
          pexp_ident ~loc { txt = Lident "field_specs"; loc }
      in
      let body =
        pexp_apply ~loc (mk_lid_expr loc (Ldot (Lident "B", "build_value_field_views")))
          [ Nolabel, fs_call
          ; Nolabel, pexp_ident ~loc { txt = Lident "ct"; loc }
          ; Nolabel, pexp_ident ~loc { txt = Lident "v"; loc }
          ; Labelled "domain_type", pexp_ident ~loc { txt = Lident "domain_type"; loc } ]
      in
      let inner =
        pexp_fun ~loc Nolabel None
          (ppat_var ~loc { txt = "ct"; loc })
          (pexp_fun ~loc Nolabel None
             (ppat_var ~loc { txt = "v"; loc })
             body)
      in
      let inner2 =
        pexp_fun ~loc (Optional "domain_type")
          (Some (pexp_apply ~loc (mk_lid_expr loc (Ldot (Lident "B", "domain_type_of_name")))
                   [Nolabel, mk_str loc "DTDefault"]))
          (ppat_var ~loc { txt = "domain_type"; loc })
          inner
      in
      let expr =
        if has_time_field then
          pexp_fun ~loc (Labelled "format_time")
            None
            (ppat_var ~loc { txt = "format_time"; loc })
            inner2
        else inner2
      in
      pstr_value ~loc Nonrecursive [{
          pvb_pat = ppat_var ~loc { txt = "build_value_fields"; loc };
          pvb_expr = expr;
          pvb_attributes = [];
          pvb_loc = loc;
          pvb_constraint = None }]
    in

    (* --- build_patch_fields binding --- *)
    let build_patch_fields_binding =
      let fs_call =
        if has_time_field then
          pexp_apply ~loc (pexp_ident ~loc { txt = Lident "field_specs"; loc })
            [Labelled "format_time", pexp_ident ~loc { txt = Lident "format_time"; loc }]
        else
          pexp_ident ~loc { txt = Lident "field_specs"; loc }
      in
      let body =
        pexp_apply ~loc (mk_lid_expr loc (Ldot (Lident "B", "build_patch_field_views")))
          [ Nolabel, fs_call
          ; Nolabel, pexp_ident ~loc { txt = Lident "p"; loc }
          ; Labelled "domain_type", pexp_ident ~loc { txt = Lident "domain_type"; loc } ]
      in
      let inner =
        pexp_fun ~loc Nolabel None
          (ppat_var ~loc { txt = "p"; loc })
          body
      in
      let inner2 =
        pexp_fun ~loc (Optional "domain_type")
          (Some (pexp_apply ~loc (mk_lid_expr loc (Ldot (Lident "B", "domain_type_of_name")))
                   [Nolabel, mk_str loc "DTDefault"]))
          (ppat_var ~loc { txt = "domain_type"; loc })
          inner
      in
      let expr =
        if has_time_field then
          pexp_fun ~loc (Labelled "format_time")
            None
            (ppat_var ~loc { txt = "format_time"; loc })
            inner2
        else inner2
      in
      pstr_value ~loc Nonrecursive [{
          pvb_pat = ppat_var ~loc { txt = "build_patch_fields"; loc };
          pvb_expr = expr;
          pvb_attributes = [];
          pvb_loc = loc;
          pvb_constraint = None }]
    in

    (* --- build_item binding --- *)
    let build_item_binding =
      let specs_call_args =
        let time_args = if has_time_field then
            [Labelled "format_time", pexp_ident ~loc { txt = Lident "format_time"; loc }]
          else [] in
        let builder_args = List.map builder_fields ~f:(fun (_, bname) ->
            Labelled bname, pexp_ident ~loc { txt = Lident bname; loc }) in
        time_args @ builder_args
      in
      let specs_arg =
        if specs_call_args = [] then
          pexp_ident ~loc { txt = Lident "section_specs"; loc }
        else
          pexp_apply ~loc (pexp_ident ~loc { txt = Lident "section_specs"; loc })
            specs_call_args
      in
      let body =
        pexp_apply ~loc (mk_lid_expr loc (Ldot (Lident "B", "build_item_from_specs")))
          [ Labelled "name", pexp_ident ~loc { txt = Lident "name"; loc }
          ; Labelled "domain_type", pexp_ident ~loc { txt = Lident "domain_type"; loc }
          ; Labelled "specs", specs_arg
          ; Nolabel, pexp_ident ~loc { txt = Lident "c"; loc } ]
      in
      let inner =
        pexp_fun ~loc Nolabel None
          (ppat_var ~loc { txt = "c"; loc })
          body
      in
      let inner2 =
        pexp_fun ~loc (Optional "domain_type")
          (Some (pexp_apply ~loc (mk_lid_expr loc (Ldot (Lident "B", "domain_type_of_name")))
                   [Nolabel, mk_str loc "DTDefault"]))
          (ppat_var ~loc { txt = "domain_type"; loc })
          inner
      in
      let inner3 =
        pexp_fun ~loc (Optional "name")
          (Some (mk_str loc ""))
          (ppat_var ~loc { txt = "name"; loc })
          inner2
      in
      let expr =
        let base =
          if has_time_field then
            pexp_fun ~loc (Labelled "format_time")
              None
              (ppat_var ~loc { txt = "format_time"; loc })
              inner3
          else inner3
        in
        List.fold_left (List.rev builder_fields) ~init:base
          ~f:(fun acc (_, bname) ->
              pexp_fun ~loc (Labelled bname)
                None
                (ppat_var ~loc { txt = bname; loc })
                acc)
      in
      pstr_value ~loc Nonrecursive [{
          pvb_pat = ppat_var ~loc { txt = "build_item"; loc };
          pvb_expr = expr;
          pvb_attributes = [];
          pvb_loc = loc;
          pvb_constraint = None }]
    in

    (* --- build_section_name binding --- *)
    let build_section_name_binding =
      if has_naming then [generate_build_section_name ~loc ni] else []
    in

    (* --- functor module --- *)
    let b_sig = pmty_ident ~loc { loc; txt = Longident.parse "Alsdiff_view_spec_types.View_spec_types.S" } in
    let functor_param = Named ({ txt = Some "B"; loc }, b_sig) in
    let body_mod = pmod_structure ~loc (
        inline_bindings @ build_section_name_binding @ [
          field_specs_binding;
          section_specs_binding;
          build_value_fields_binding;
          build_patch_fields_binding;
          build_item_binding;
        ]) in
    let functor_mod = pmod_functor ~loc functor_param body_mod in
    [pstr_module ~loc {
        pmb_name = { txt = Some "ViewSpec"; loc };
        pmb_expr = functor_mod;
        pmb_attributes = [];
        pmb_loc = loc }]

let impl_generator = Deriving.Generator.V2.make_noarg generate_view_spec_impl

let deriver =
  Deriving.add "view_spec" ~str_type_decl:impl_generator
