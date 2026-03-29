open Alsdiff_base.Diff

module Inner = struct
  type t = { x : int; y : float } [@@deriving eq, patch] [@@patch.generate_diff]
end

module TestVariant = struct
  type t =
    | Simple of int
    | Complex of Inner.t
    | Multi of float * string
    | Empty
  [@@deriving eq, patch] [@@patch.generate_diff]
end

module Container = struct
  type t = {
    name : string;
    variant_field : TestVariant.t;
  } [@@deriving eq, patch] [@@patch.generate_diff]
end

(* is_empty tests *)

let test_is_empty_simple_unchanged () =
  let patch = TestVariant.diff (Simple 42) (Simple 42) in
  Alcotest.(check bool) "simple unchanged" true (TestVariant.Patch.is_empty patch)

let test_is_empty_simple_modified () =
  let patch = TestVariant.diff (Simple 1) (Simple 2) in
  Alcotest.(check bool) "simple modified" false (TestVariant.Patch.is_empty patch)

let test_is_empty_complex_unchanged () =
  let inner = { Inner.x = 1; y = 2.0 } in
  let patch = TestVariant.diff (Complex inner) (Complex inner) in
  Alcotest.(check bool) "complex unchanged" true (TestVariant.Patch.is_empty patch)

let test_is_empty_complex_modified () =
  let old_inner = { Inner.x = 1; y = 2.0 } in
  let new_inner = { Inner.x = 1; y = 3.0 } in
  let patch = TestVariant.diff (Complex old_inner) (Complex new_inner) in
  Alcotest.(check bool) "complex modified" false (TestVariant.Patch.is_empty patch)

let test_is_empty_multi_unchanged () =
  let patch = TestVariant.diff (Multi (1.0, "a")) (Multi (1.0, "a")) in
  Alcotest.(check bool) "multi unchanged" true (TestVariant.Patch.is_empty patch)

let test_is_empty_multi_modified () =
  let patch = TestVariant.diff (Multi (1.0, "a")) (Multi (2.0, "a")) in
  Alcotest.(check bool) "multi modified" false (TestVariant.Patch.is_empty patch)

let test_is_empty_empty () =
  let patch = TestVariant.diff Empty Empty in
  Alcotest.(check bool) "empty unchanged" true (TestVariant.Patch.is_empty patch)

(* diff tests *)

let test_diff_simple_unchanged () =
  let patch = TestVariant.diff (Simple 42) (Simple 42) in
  let is_empty = match patch with
    | Simple p -> is_unchanged_atomic_update p
    | _ -> false
  in
  Alcotest.(check bool) "simple unchanged" true is_empty

let test_diff_simple_modified () =
  let patch = TestVariant.diff (Simple 1) (Simple 2) in
  match patch with
  | Simple p ->
    (match p with `Modified { oldval; newval } ->
       Alcotest.(check int) "old" 1 oldval;
       Alcotest.(check int) "new" 2 newval
                | _ -> Alcotest.fail "Expected Modified")
  | _ -> Alcotest.fail "Expected Simple"

let test_diff_complex_modified () =
  let old_inner = { Inner.x = 1; y = 2.0 } in
  let new_inner = { Inner.x = 1; y = 3.0 } in
  let patch = TestVariant.diff (Complex old_inner) (Complex new_inner) in
  match patch with
  | Complex p ->
    Alcotest.(check bool) "complex patch not empty" false (is_unchanged_update (module Inner.Patch) p)
  | _ -> Alcotest.fail "Expected Complex"

let test_diff_empty () =
  let patch = TestVariant.diff Empty Empty in
  Alcotest.(check bool) "empty patch is Empty" true (match patch with Empty -> true | _ -> false)

let test_diff_multi_mixed () =
  let patch = TestVariant.diff (Multi (1.0, "a")) (Multi (2.0, "a")) in
  match patch with
  | Multi (fp, sp) ->
    Alcotest.(check bool) "float modified" false (is_unchanged_atomic_update fp);
    Alcotest.(check bool) "string unchanged" true (is_unchanged_atomic_update sp)
  | _ -> Alcotest.fail "Expected Multi"

(* Constructor mismatch tests *)

let test_diff_mismatch_simple_complex () =
  match TestVariant.diff (Simple 1) (Complex { Inner.x = 1; y = 2.0 }) with
  | _ -> Alcotest.fail "Should have raised Failure"
  | exception Failure _ -> ()

let test_diff_mismatch_empty_simple () =
  match TestVariant.diff Empty (Simple 1) with
  | _ -> Alcotest.fail "Should have raised Failure"
  | exception Failure _ -> ()

(* Container integration tests *)

let test_container_variant_unchanged () =
  let c1 = { Container.name = "test"; variant_field = Simple 42 } in
  let c2 = { Container.name = "test"; variant_field = Simple 42 } in
  let patch = Container.diff c1 c2 in
  Alcotest.(check bool) "container unchanged" true (Container.Patch.is_empty patch)

let test_container_variant_modified () =
  let c1 = { Container.name = "test"; variant_field = Simple 1 } in
  let c2 = { Container.name = "test"; variant_field = Simple 2 } in
  let patch = Container.diff c1 c2 in
  Alcotest.(check bool) "container modified" false (Container.Patch.is_empty patch)

let test_container_variant_constructor_change () =
  let c1 = { Container.name = "test"; variant_field = Simple 1 } in
  let c2 = { Container.name = "test"; variant_field = Empty } in
  match Container.diff c1 c2 with
  | _ -> Alcotest.fail "Should have raised Failure"
  | exception Failure _ -> ()

let test_container_name_and_variant_change () =
  let c1 = { Container.name = "old"; variant_field = Multi (1.0, "a") } in
  let c2 = { Container.name = "new"; variant_field = Multi (2.0, "b") } in
  let patch = Container.diff c1 c2 in
  Alcotest.(check bool) "container modified" false (Container.Patch.is_empty patch)

let () =
  Alcotest.run "VariantPatch" [
    "is_empty", [
      Alcotest.test_case "simple unchanged" `Quick test_is_empty_simple_unchanged;
      Alcotest.test_case "simple modified" `Quick test_is_empty_simple_modified;
      Alcotest.test_case "complex unchanged" `Quick test_is_empty_complex_unchanged;
      Alcotest.test_case "complex modified" `Quick test_is_empty_complex_modified;
      Alcotest.test_case "multi unchanged" `Quick test_is_empty_multi_unchanged;
      Alcotest.test_case "multi modified" `Quick test_is_empty_multi_modified;
      Alcotest.test_case "empty" `Quick test_is_empty_empty;
    ];
    "diff", [
      Alcotest.test_case "simple unchanged" `Quick test_diff_simple_unchanged;
      Alcotest.test_case "simple modified" `Quick test_diff_simple_modified;
      Alcotest.test_case "complex modified" `Quick test_diff_complex_modified;
      Alcotest.test_case "empty" `Quick test_diff_empty;
      Alcotest.test_case "multi mixed" `Quick test_diff_multi_mixed;
    ];
    "mismatch", [
      Alcotest.test_case "simple vs complex" `Quick test_diff_mismatch_simple_complex;
      Alcotest.test_case "empty vs simple" `Quick test_diff_mismatch_empty_simple;
    ];
    "container", [
      Alcotest.test_case "variant unchanged" `Quick test_container_variant_unchanged;
      Alcotest.test_case "variant modified" `Quick test_container_variant_modified;
      Alcotest.test_case "constructor change fails" `Quick test_container_variant_constructor_change;
      Alcotest.test_case "name and variant change" `Quick test_container_name_and_variant_change;
    ];
  ]
