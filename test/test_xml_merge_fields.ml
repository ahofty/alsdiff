open Alsdiff_base

let xml_of_attrs name attrs childs =
  Xml.Element { name; attrs; childs }

module Pc = Alsdiff_merge.Patch_compare
module Xmf = Alsdiff_merge.Xml_merge_fields

let test_all_fields_ours () =
  let base = xml_of_attrs "Track" [("Name", "Drums")]
      [xml_of_attrs "Mixer" [] [Xml.Data "base_mixer"]]
  in
  let ours = xml_of_attrs "Track" [("Name", "DRUMS")]
      [xml_of_attrs "Mixer" [] [Xml.Data "ours_mixer"]]
  in
  let theirs = xml_of_attrs "Track" [("Name", "drums")]
      [xml_of_attrs "Mixer" [] [Xml.Data "theirs_mixer"]]
  in
  let fields = [
    Pc.{ field_name = "name"; action = Take_ours; xml_loc = Some (Attr "Name") };
    Pc.{ field_name = "mixer"; action = Take_ours; xml_loc = Some (Child "Mixer") };
  ] in
  let resolutions = Hashtbl.create 4 in
  let result = Xmf.merge_from_fields ~base ~ours ~theirs ~fields ~resolutions in
  Alcotest.(check bool) "Name attribute from ours"
    true (Xml.get_attr "Name" result = "DRUMS");
  Alcotest.(check bool) "Mixer child from ours"
    true (List.exists (fun c ->
        match c with
        | Xml.Element { name; childs = [Xml.Data "ours_mixer"]; _ } ->
          String.equal name "Mixer"
        | _ -> false
      ) (Xml.get_childs result))

let test_all_fields_theirs () =
  let base = xml_of_attrs "Track" [("Name", "Drums")]
      [xml_of_attrs "Mixer" [] [Xml.Data "base_mixer"]]
  in
  let ours = xml_of_attrs "Track" [("Name", "DRUMS")]
      [xml_of_attrs "Mixer" [] [Xml.Data "ours_mixer"]]
  in
  let theirs = xml_of_attrs "Track" [("Name", "drums")]
      [xml_of_attrs "Mixer" [] [Xml.Data "theirs_mixer"]]
  in
  let fields = [
    Pc.{ field_name = "name"; action = Take_theirs; xml_loc = Some (Attr "Name") };
    Pc.{ field_name = "mixer"; action = Take_theirs; xml_loc = Some (Child "Mixer") };
  ] in
  let resolutions = Hashtbl.create 4 in
  let result = Xmf.merge_from_fields ~base ~ours ~theirs ~fields ~resolutions in
  Alcotest.(check bool) "Name attribute from theirs"
    true (Xml.get_attr "Name" result = "drums")

let test_mixed_fields () =
  let base = xml_of_attrs "Track" [("Name", "Drums")]
      [xml_of_attrs "Mixer" [] [Xml.Data "base_mixer"]]
  in
  let ours = xml_of_attrs "Track" [("Name", "DRUMS")]
      [xml_of_attrs "Mixer" [] [Xml.Data "ours_mixer"]]
  in
  let theirs = xml_of_attrs "Track" [("Name", "drums")]
      [xml_of_attrs "Mixer" [] [Xml.Data "theirs_mixer"]]
  in
  let fields = [
    Pc.{ field_name = "name"; action = Take_ours; xml_loc = Some (Attr "Name") };
    Pc.{ field_name = "mixer"; action = Take_theirs; xml_loc = Some (Child "Mixer") };
  ] in
  let resolutions = Hashtbl.create 4 in
  let result = Xmf.merge_from_fields ~base ~ours ~theirs ~fields ~resolutions in
  Alcotest.(check bool) "Name from ours"
    true (Xml.get_attr "Name" result = "DRUMS");
  Alcotest.(check bool) "Mixer from theirs"
    true (List.exists (fun c ->
        match c with
        | Xml.Element { name; childs = [Xml.Data "theirs_mixer"]; _ } ->
          String.equal name "Mixer"
        | _ -> false
      ) (Xml.get_childs result))

let test_unresolved_fallback_ours () =
  let base = xml_of_attrs "Track" [("Name", "Drums")] [] in
  let ours = xml_of_attrs "Track" [("Name", "DRUMS")] [] in
  let theirs = xml_of_attrs "Track" [("Name", "drums")] [] in
  let conflict : Alsdiff_merge.Conflict.t = {
    Alsdiff_merge.Conflict.path = "test/name";
    kind = Alsdiff_merge.Conflict.Atomic_conflict;
    ours_desc = "DRUMS";
    theirs_desc = "drums";
  } in
  let fields = [
    Pc.{ field_name = "name"; action = Conflict conflict; xml_loc = Some (Attr "Name") };
  ] in
  let resolutions = Hashtbl.create 4 in
  let result = Xmf.merge_from_fields ~base ~ours ~theirs ~fields ~resolutions in
  Alcotest.(check bool) "Unresolved conflict defaults to ours"
    true (Xml.get_attr "Name" result = "DRUMS")

let test_generic_merge_attribute () =
  let base = xml_of_attrs "Track" [("Name", "Drums"); ("Id", "1")] [] in
  let ours = xml_of_attrs "Track" [("Name", "DRUMS"); ("Id", "1")] [] in
  let theirs = xml_of_attrs "Track" [("Name", "drums"); ("Id", "1")] [] in
  let resolutions = Hashtbl.create 4 in
  Hashtbl.replace resolutions "test/Name" Alsdiff_merge.Conflict.Theirs;
  let result = Xmf.merge_generic ~base ~ours ~theirs ~resolutions
      ~conflict_path:"test" in
  Alcotest.(check bool) "Generic merge picks theirs for Name"
    true (Xml.get_attr "Name" result = "drums")

let () =
  Alcotest.run "xml_merge_fields" [
    "all fields ours", [
      Alcotest.test_case "All fields take ours" `Quick test_all_fields_ours;
    ];
    "all fields theirs", [
      Alcotest.test_case "All fields take theirs" `Quick test_all_fields_theirs;
    ];
    "mixed fields", [
      Alcotest.test_case "Name ours, mixer theirs" `Quick test_mixed_fields;
    ];
    "unresolved fallback", [
      Alcotest.test_case "Unresolved conflict defaults to ours" `Quick test_unresolved_fallback_ours;
    ];
    "generic merge", [
      Alcotest.test_case "Generic merge with attribute diff" `Quick test_generic_merge_attribute;
    ];
  ]
