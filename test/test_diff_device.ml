
open Alsdiff_base.Xml
open Alsdiff_live
open Alsdiff_live.Device

(* Helper to create a dummy DeviceParam *)
let make_param name value =
  {
    DeviceParam.base = {
      GenericParam.name = name;
      value = value;
      automation = 0;
      modulation = 0;
      mapping = None;
    };
  }

(* Helper to create a dummy Regular Device *)
let make_regular_device id name params =
  Regular {
    id = id;
    device_name = name;
    display_name = name;
    pointee = id;
    enabled = make_param "Device On" (Bool true);
    params = params;
    preset = None;
  }

let test_regular_device_diff () =
  let param1 = make_param "Drive" (Float 0.5) in
  let param2 = make_param "Tone" (Float 0.2) in

  let old_device = make_regular_device 1 "Overdrive" [param1; param2] in

  let param1_mod = { DeviceParam.base = { param1.base with Device.GenericParam.value = Float 0.8 } } in
  let new_device = make_regular_device 1 "Overdrive" [param1_mod; param2] in

  let patch = Device.diff old_device new_device in

  (* Check that we got a RegularPatch *)
  (match patch with
   | Device.Patch.RegularPatch regular_patch ->
     (* The first param (Drive) should be Modified, second (Tone) should be Unchanged *)
     (* Check the Drive param change (first in params list) *)
     (match regular_patch.params with
      | drive_change :: _ ->
        (match drive_change with
         | `Modified param_patch ->
           (match param_patch.Device.DeviceParam.Patch.base with
            | `Modified generic_patch ->
              (match generic_patch.Device.GenericParam.Patch.value with
               | `Modified v ->
                 (match v.oldval with
                  | Device.Float old_val -> Alcotest.(check (float 0.01)) "old value" 0.5 old_val
                  | _ -> Alcotest.fail "Expected Float value");
                 (match v.newval with
                  | Device.Float new_val -> Alcotest.(check (float 0.01)) "new value" 0.8 new_val
                  | _ -> Alcotest.fail "Expected Float value")
               | _ -> Alcotest.fail "Expected value to be modified")
            | _ -> Alcotest.fail "Expected base to be modified")
         | _ -> Alcotest.fail "Expected param to be modified")
      | _ -> Alcotest.fail "Expected at least one param change")
   | _ -> Alcotest.fail "Expected RegularPatch")

let test_plugin_device_diff () =
  (* Mock Plugin Device *)
  (* Test that plugin device diffing works correctly *)
  let make_plugin_param id name _index value =
    {
      PluginParam.id = id;
      base = {
        GenericParam.name = name;
        value = value;
        automation = 0;
        modulation = 0;
        mapping = None;
      };
    }
  in

  let old_plugin = Plugin {
      id = 2;
      device_name = "VST Plugin";
      display_name = "MySynth";
      pointee = 2;
      enabled = make_param "Device On" (Bool true);
      desc = {
        PluginDesc.name = "MySynth";
        uid = "vst3:mysynth";
        plugin_type = PluginDesc.Vst3;
        processor_state = "";
      };
      params = [make_plugin_param 1 "Cutoff" 1 (Float 0.5)];
      preset = None;
    } in

  let new_plugin = Plugin {
      id = 2;
      device_name = "VST Plugin";
      display_name = "MySynth";
      pointee = 2;
      enabled = make_param "Device On" (Bool true);
      desc = {
        PluginDesc.name = "MySynth";
        uid = "vst3:mysynth";
        plugin_type = PluginDesc.Vst3;
        processor_state = "";
      };
      params = [make_plugin_param 1 "Cutoff" 1 (Float 0.7)];
      preset = None;
    } in

  let patch = Device.diff old_plugin new_plugin in

  (* Check that we got a PluginPatch *)
  (match patch with
   | Device.Patch.PluginPatch plugin_patch ->
     (* The Cutoff param should be Modified *)
     (* Check the Cutoff param change (first in params list) *)
     (match plugin_patch.params with
      | cutoff_change :: _ ->
        (match cutoff_change with
         | `Modified param_patch ->
           (match param_patch.Device.PluginParam.Patch.base with
            | `Modified generic_patch ->
              (match generic_patch.Device.GenericParam.Patch.value with
               | `Modified v ->
                 (match v.oldval with
                  | Device.Float old_val -> Alcotest.(check (float 0.01)) "old value" 0.5 old_val
                  | _ -> Alcotest.fail "Expected Float value");
                 (match v.newval with
                  | Device.Float new_val -> Alcotest.(check (float 0.01)) "new value" 0.7 new_val
                  | _ -> Alcotest.fail "Expected Float value")
               | _ -> Alcotest.fail "Expected value to be modified")
            | _ -> Alcotest.fail "Expected base to be modified")
         | _ -> Alcotest.fail "Expected param to be modified")
      | _ -> Alcotest.fail "Expected at least one param change")
   | _ -> Alcotest.fail "Expected PluginPatch")

let test_group_device_xml_path = Utils.resolve_test_data_path "group_device.xml"

let test_m4l_device_xml_path = Utils.resolve_test_data_path "m4l_device.xml"

(* ==================== GroupDevice Diff Tests ==================== *)

let test_group_device_diff_no_change () =
  let xml = read_file test_group_device_xml_path in
  let device = Device.create xml in
  let group = match device with Device.Group g -> g | _ -> failwith "Expected Group" in
  let patch = Device.GroupDevice.diff group group in
  Alcotest.(check bool) "group patch is empty" true (Device.GroupDevice.Patch.is_empty patch)

let test_group_device_diff_display_name () =
  let xml = read_file test_group_device_xml_path in
  let device = Device.create xml in
  let group = match device with Device.Group g -> g | _ -> failwith "Expected Group" in
  let modified_group = { group with display_name = "Modified Name" } in
  let patch = Device.GroupDevice.diff group modified_group in
  Alcotest.(check bool) "group patch is not empty" false (Device.GroupDevice.Patch.is_empty patch)

let test_group_device_diff_macro_change () =
  let xml = read_file test_group_device_xml_path in
  let device = Device.create xml in
  let group = match device with Device.Group g -> g | _ -> failwith "Expected Group" in
  let modified_macros = match group.macros with
    | [] -> failwith "Expected at least one macro"
    | first :: rest ->
      let modified_first = {
        first with
        base = { first.base with Device.GenericParam.value = Device.Float 100.0 };
      } in
      modified_first :: rest
  in
  let modified_group = { group with macros = modified_macros } in
  let patch = Device.GroupDevice.diff group modified_group in
  Alcotest.(check bool) "group patch is not empty" false (Device.GroupDevice.Patch.is_empty patch)

(* ==================== Max4LiveDevice Diff Tests ==================== *)

let test_m4l_device_diff_no_change () =
  let xml = read_file test_m4l_device_xml_path in
  let device = Device.create xml in
  let m4l = match device with Device.Max4Live m -> m | _ -> failwith "Expected Max4Live" in
  let patch = Device.Max4LiveDevice.diff m4l m4l in
  Alcotest.(check bool) "m4l patch is empty" true (Device.Max4LiveDevice.Patch.is_empty patch)

let test_m4l_device_diff_param_change () =
  let xml = read_file test_m4l_device_xml_path in
  let device = Device.create xml in
  let m4l = match device with Device.Max4Live m -> m | _ -> failwith "Expected Max4Live" in
  let modified_params = match m4l.params with
    | [] -> failwith "Expected at least one param"
    | first :: rest ->
      let modified_first = {
        first with
        base = { first.base with Device.GenericParam.value = Device.Float 0.75 };
      } in
      modified_first :: rest
  in
  let modified_m4l = { m4l with params = modified_params } in
  let patch = Device.Max4LiveDevice.diff m4l modified_m4l in
  Alcotest.(check bool) "m4l patch is not empty" false (Device.Max4LiveDevice.Patch.is_empty patch)

(* ==================== Device.Patch.is_empty Tests ==================== *)

let test_device_patch_is_empty_no_change () =
  let param1 = make_param "Drive" (Float 0.5) in
  let param2 = make_param "Tone" (Float 0.2) in
  let device = make_regular_device 1 "Overdrive" [param1; param2] in
  let patch = Device.diff device device in
  Alcotest.(check bool) "device patch is empty" true (Device.Patch.is_empty patch)

let test_device_patch_is_not_empty_with_change () =
  let param1 = make_param "Drive" (Float 0.5) in
  let param2 = make_param "Tone" (Float 0.2) in
  let old_device = make_regular_device 1 "Overdrive" [param1; param2] in
  let param1_mod = { DeviceParam.base = { param1.base with Device.GenericParam.value = Float 0.8 } } in
  let new_device = make_regular_device 1 "Overdrive" [param1_mod; param2] in
  let patch = Device.diff old_device new_device in
  Alcotest.(check bool) "device patch is not empty" false (Device.Patch.is_empty patch)

(* ==================== Branch Diff Test ==================== *)

let test_branch_diff_no_change () =
  let xml = read_file test_group_device_xml_path in
  let device = Device.create xml in
  let group = match device with Device.Group g -> g | _ -> failwith "Expected Group" in
  match group.branches with
  | [] -> Alcotest.fail "Expected at least one branch"
  | branch :: _ ->
    let patch = Device.Branch.diff branch branch in
    Alcotest.(check bool) "branch patch is empty" true (Device.Branch.Patch.is_empty patch)

let () =
  Alcotest.run "Diff Device" [
    "regular-device", [
      Alcotest.test_case "Test regular device diff" `Quick test_regular_device_diff;
    ];
    "plugin-device", [
      Alcotest.test_case "Test plugin device diff" `Quick test_plugin_device_diff;
    ];
    "group-device", [
      Alcotest.test_case "GroupDevice diff no change" `Quick test_group_device_diff_no_change;
      Alcotest.test_case "GroupDevice diff display name change" `Quick test_group_device_diff_display_name;
      Alcotest.test_case "GroupDevice diff macro change" `Quick test_group_device_diff_macro_change;
    ];
    "m4l-device", [
      Alcotest.test_case "Max4LiveDevice diff no change" `Quick test_m4l_device_diff_no_change;
      Alcotest.test_case "Max4LiveDevice diff param change" `Quick test_m4l_device_diff_param_change;
    ];
    "device-patch", [
      Alcotest.test_case "Device.Patch.is_empty with no change" `Quick test_device_patch_is_empty_no_change;
      Alcotest.test_case "Device.Patch.is_empty with change" `Quick test_device_patch_is_not_empty_with_change;
    ];
    "branch", [
      Alcotest.test_case "Branch.diff no change" `Quick test_branch_diff_no_change;
    ];
  ]
