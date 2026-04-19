open Alsdiff_base

let xml_of_attrs name attrs childs =
  Xml.Element { name; attrs; childs }

module Fd = Alsdiff_merge.Xml_compare

module Compare_field_diff = struct
  type t = Fd.field_diff
  let equal a b =
    String.equal a.Fd.field_name b.Fd.field_name
    && a.Fd.kind = b.Fd.kind
    && Option.equal String.equal a.Fd.ours_value b.Fd.ours_value
    && Option.equal String.equal a.Fd.theirs_value b.Fd.theirs_value
    && Option.equal String.equal a.Fd.base_value b.Fd.base_value
  let pp fmt d =
    Fmt.pf fmt "{ field_name=%s; ours=%a; theirs=%a; base=%a }"
      d.Fd.field_name
      (Fmt.option ~none:(fun fmt () -> Fmt.string fmt "None") Fmt.string)
      d.Fd.ours_value
      (Fmt.option ~none:(fun fmt () -> Fmt.string fmt "None") Fmt.string)
      d.Fd.theirs_value
      (Fmt.option ~none:(fun fmt () -> Fmt.string fmt "None") Fmt.string)
      d.Fd.base_value
end

let test_attribute_diffs () =
  let base = xml_of_attrs "Track" [("Name", "Drums"); ("Id", "1")] [] in
  let ours = xml_of_attrs "Track" [("Name", "DRUMS"); ("Id", "1")] [] in
  let theirs = xml_of_attrs "Track" [("Name", "drums"); ("Id", "1")] [] in
  let diffs = Fd.compare_three_way ~base ~ours ~theirs in
  let name_diff = List.find_opt (fun d -> d.Fd.field_name = "Name") diffs in
  Alcotest.(check (option (module Compare_field_diff)))
    "Name attribute differs"
    (Some { Fd.field_name = "Name"; kind = Fd.Attribute_diff;
            ours_value = Some "DRUMS"; theirs_value = Some "drums";
            base_value = Some "Drums" })
    name_diff;
  let id_diff = List.find_opt (fun d -> d.Fd.field_name = "Id") diffs in
  Alcotest.(check bool) "Id attribute unchanged" true (id_diff = None)

let test_child_element_diffs () =
  let base = xml_of_attrs "Track" []
      [xml_of_attrs "Mixer" [] [Xml.Data "base_mixer"];
       xml_of_attrs "Name" [] [Xml.Data "Drums"]]
  in
  let ours = xml_of_attrs "Track" []
      [xml_of_attrs "Mixer" [] [Xml.Data "ours_mixer"];
       xml_of_attrs "Name" [] [Xml.Data "Drums"]]
  in
  let theirs = xml_of_attrs "Track" []
      [xml_of_attrs "Mixer" [] [Xml.Data "theirs_mixer"];
       xml_of_attrs "Name" [] [Xml.Data "Drums"]]
  in
  let diffs = Fd.compare_three_way ~base ~ours ~theirs in
  let mixer_diff = List.find_opt (fun d -> d.Fd.field_name = "Mixer") diffs in
  Alcotest.(check bool) "Mixer child differs" true (mixer_diff <> None);
  let name_diff = List.find_opt (fun d -> d.Fd.field_name = "Name") diffs in
  Alcotest.(check bool) "Name child unchanged" true (name_diff = None)

let test_identical_elements () =
  let el = xml_of_attrs "Track" [("Name", "Drums")] [] in
  let diffs = Fd.compare_three_way ~base:el ~ours:el ~theirs:el in
  Alcotest.(check int) "No diffs for identical elements" 0 (List.length diffs)

let test_added_removed_children () =
  let base = xml_of_attrs "Track" [] [] in
  let ours = xml_of_attrs "Track" []
      [xml_of_attrs "DeviceChain" [] []]
  in
  let theirs = xml_of_attrs "Track" []
      [xml_of_attrs "Mixer" [] []]
  in
  let diffs = Fd.compare_three_way ~base ~ours ~theirs in
  Alcotest.(check int) "Two child diffs" 2 (List.length diffs)

let () =
  Alcotest.run "xml_compare" [
    "attribute diffs", [
      Alcotest.test_case
        "Three elements with differing attributes" `Quick test_attribute_diffs;
    ];
    "child element diffs", [
      Alcotest.test_case
        "Three elements with differing child elements" `Quick test_child_element_diffs;
    ];
    "identical elements", [
      Alcotest.test_case
        "Identical elements produce empty diff" `Quick test_identical_elements;
    ];
    "added/removed children", [
      Alcotest.test_case
        "Added and removed children" `Quick test_added_removed_children;
    ];
  ]
