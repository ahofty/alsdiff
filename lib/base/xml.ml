type t =
  | Element of { name: string; attrs: (string * string) list; childs: t list }
  | Data of string

exception Xml_error of t * string

let get_name = function
  | Element { name; _ } -> name
  | Data _ as xml -> raise (Xml_error (xml, "Cannot get name from XML Data node"))

let get_childs = function
  | Element { childs; _ } -> childs
  | Data _ as xml -> raise (Xml_error (xml, "Cannot get children from XML Data node"))

let get_value = function
  | Data value -> value
  | Element _ as xml -> raise (Xml_error (xml, "Cannot get value from XML Element node"))

let get_data_child = function
  | Element {childs = [Data d]; _} -> d
  | _ as xml -> raise (Xml_error (xml, "Cannot get the data child when it doesn't have a data child"))

let get_attr name xml =
  match xml with
  | Element { attrs; _ } ->
    (try List.assoc name attrs
     with Not_found ->
       raise (Xml_error (xml, Printf.sprintf "Attribute '%s' not found" name)))
  | Data _ ->
    raise (Xml_error (xml, "Cannot get attribute from XML Data node"))

let get_int_attr name xml =
  let value = get_attr name xml in
  try int_of_string value
  with Failure _ ->
    raise (Xml_error (xml, Printf.sprintf "Invalid integer value for attribute '%s': '%s'" name value))

let get_float_attr name xml =
  let value = get_attr name xml in
  try float_of_string value
  with Failure _ ->
    raise (Xml_error (xml, Printf.sprintf "Invalid float value for attribute '%s': '%s'" name value))

let get_int64_attr name xml =
  let value = get_attr name xml in
  try Int64.of_string value
  with Failure _ ->
    raise (Xml_error (xml, Printf.sprintf "Invalid int64 value for attribute '%s': '%s'" name value))

let get_attr_opt name xml =
  match xml with
  | Element { attrs; _ } ->
    List.assoc_opt name attrs
  | Data _ -> None

let get_int_attr_opt name xml =
  Option.bind (get_attr_opt name xml) int_of_string_opt

let get_float_attr_opt name xml =
  Option.bind (get_attr_opt name xml) float_of_string_opt

let get_bool_attr_opt name xml =
  Option.bind (get_attr_opt name xml)
    (fun x -> x |> String.lowercase_ascii |> bool_of_string_opt)

let get_int64_attr_opt name xml =
  Option.bind (get_attr_opt name xml) Int64.of_string_opt

module Parser = struct
  (* Helper functions for parsing XML *)
  let get_name (tag : Xmlm.tag)  =
    let ((_, name), _) = tag in name

  let get_attrs (tag : Xmlm.tag) =
    let ((_, _), attrs) = tag in
    List.map (fun a -> let ((_, k), v) = a in (k, v)) attrs

  let make_element tag childs = Element {
      name = get_name tag;
      attrs = get_attrs tag;
      childs = childs;
    }

  let make_data d = Data d

end

let read_file filename =
  let ic = In_channel.open_text filename in
  try
    let i = Xmlm.make_input ~strip:true (`Channel ic) in
    let result = Xmlm.input_doc_tree ~el:Parser.make_element ~data:Parser.make_data i |> snd in
    In_channel.close ic;
    result
  with e ->
    In_channel.close ic;
    raise e

let read_string s =
  let i = Xmlm.make_input ~strip:true (`String (0, s)) in
  Xmlm.input_doc_tree ~el:Parser.make_element ~data:Parser.make_data i |> snd

(* Escape XML special characters to entities *)
let escape_xml s =
  let len = String.length s in
  let buf = Buffer.create (len * 2) in
  String.iter (function
      | '<' -> Buffer.add_string buf "&lt;"
      | '>' -> Buffer.add_string buf "&gt;"
      | '&' -> Buffer.add_string buf "&amp;"
      | '"' -> Buffer.add_string buf "&quot;"
      | '\'' -> Buffer.add_string buf "&apos;"
      | c -> Buffer.add_char buf c
    ) s;
  Buffer.contents buf

let pp_attrs fmt attrs =
  List.iter (fun (k, v) -> Fmt.pf fmt " %s=\"%s\"" k (escape_xml v)) attrs

(* Pretty printer with indentation and newlines. *)
let pp fmt xml =
  let rec pp_element depth = function
    | Element {name; attrs; childs} ->
      let indent = String.make (depth * 2) ' ' in
      if childs = [] then
        Fmt.pf fmt "%s<%s%a/>@," indent name pp_attrs attrs
      else (
        Fmt.pf fmt "%s<%s%a>@," indent name pp_attrs attrs;
        List.iter (pp_element (depth + 1)) childs;
        Fmt.pf fmt "%s</%s>@," indent name
      )
    | Data s ->
      if s <> "" then Fmt.pf fmt "%s@," (escape_xml s)
  in
  Fmt.pf fmt "@[<v>%a@]" (fun _ () -> pp_element 0 xml) ()

(* Compact pretty printer without indentation or newlines *)
let rec pp_compact fmt = function
  | Element {name; attrs; childs} ->
    if childs = [] then
      Fmt.pf fmt "<%s%a/>" name pp_attrs attrs
    else (
      Fmt.pf fmt "<%s%a>" name pp_attrs attrs;
      List.iter (fun child -> pp_compact fmt child) childs;
      Fmt.pf fmt "</%s>" name
    )
  | Data s ->
    Fmt.pf fmt "%s" (escape_xml s)

let to_string xml =
  Fmt.str "%a" pp xml

let pp_xml_error fmt (xml, msg) =
  Fmt.pf fmt "%s@\nProblematic XML:\n%a@\n" msg pp xml

let rec equal a b =
  match a, b with
  | Element { name = n1; attrs = a1; childs = c1 },
    Element { name = n2; attrs = a2; childs = c2 } ->
    String.equal n1 n2
    && List.equal
      (fun (k1, v1) (k2, v2) -> String.equal k1 k2 && String.equal v1 v2)
      a1 a2
    && List.equal equal c1 c2
  | Data s1, Data s2 -> String.equal s1 s2
  | _ -> false

let replace_child tree ~old ~replacement =
  match tree with
  | Element { name; attrs; childs } ->
    let childs =
      List.map (fun c -> if c == old then replacement else c) childs
    in
    Element { name; attrs; childs }
  | Data _ -> tree

let remove_child tree ~child =
  match tree with
  | Element { name; attrs; childs } ->
    let childs = List.filter (fun c -> c != child) childs in
    Element { name; attrs; childs }
  | Data _ -> tree

let add_child tree child =
  match tree with
  | Element { name; attrs; childs } ->
    Element { name; attrs; childs = childs @ [ child ] }
  | Data _ -> tree

let insert_child_before tree ~reference ~new_child =
  match tree with
  | Element { name; attrs; childs } ->
    let childs =
      List.concat_map
        (fun c -> if c == reference then [ new_child; c ] else [ c ])
        childs
    in
    Element { name; attrs; childs }
  | Data _ -> tree

let rec update_descendant ~root ~target ~f =
  if root == target then f root
  else
    match root with
    | Element { name; attrs; childs } ->
      let childs =
        List.map (fun c -> update_descendant ~root:c ~target ~f) childs
      in
      Element { name; attrs; childs }
    | Data _ -> root
