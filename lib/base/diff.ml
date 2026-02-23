open Equality

module type PATCH = sig
  type t
  val is_empty : t -> bool
end

module type DIFFABLE_ID = sig
  type t
  include IDENTIFIABLE with type t := t

  module Patch : PATCH

  val diff : t -> t -> Patch.t
end

module type DIFFABLE_EQ = sig
  type t
  include EQUALABLE with type t := t

  module Patch : PATCH

  val diff : t -> t -> Patch.t
end


(** Phantom types for distinguishing change kinds *)
type atomic
type structured

(** The unified type to describe the change of a value.

    This polymorphic variant type represents different kinds of changes that can occur
    when comparing two values. The ['kind] parameter is a phantom type that distinguishes
    between atomic and structured changes at the type level.

    @param 'a The type of the value being changed
    @param 'p The type of the patch/modify information
    @param 'kind The phantom type (either [atomic] or [structured])

    TODO: adding a [`Moved] or [`Reordered] variant,
    currently the Myers diff algorithm can't really detect an item moved/reordered in a sequence.
*)
type ('a, 'p, 'kind) change = [
  | `Unchanged  (** The value remained the same *)
  | `Added of 'a  (** A new value was added *)
  | `Removed of 'a  (** A value was removed *)
  | `Modified of 'p  (** A value was modified, with patch information [p] *)
]

(** A patch representing the change of a simple atomic value.

    Atomic patches are used for primitive values that can be directly compared
    and replaced, such as integers, strings, floats, or booleans.

    @param oldval The original value before the change
    @param newval The new value after the change
*)
type 'a atomic_patch = { oldval : 'a; newval : 'a }

(** A change type for atomic values with compile-time type safety.

    This type ensures that only atomic changes (changes to simple/primitive values)
    can be used, preventing accidental mixing with structured changes at compile time.

    @param 'a The type of the atomic value (e.g., int, string, float)
*)
type 'a atomic_change = ('a, 'a atomic_patch, atomic) change

(** A change type for structured values with compile-time type safety.

    This type is used for complex objects that have their own Patch.t type
    and require nested diffing. Examples include Loop, Send, Device, etc.
    The phantom type [structured] prevents mixing with atomic changes.

    @param 'a The type of the structured value
    @param 'p The patch type for the structured value
*)
type ('a, 'p) structured_change = ('a, 'p, structured) change

(** A type representing updates to complex structured objects.

    Updates are used when a complex object has been modified internally,
    with the patch describing the specific changes made to the object's structure.

    @param 'p The patch type describing the modifications
    @param 'kind The phantom type (either [atomic] or [structured])
*)
type ('p, 'kind) update = [
  | `Unchanged  (** The object remained unchanged *)
  | `Modified of 'p  (** The object was modified according to patch [p] *)
]

(** An update type specifically for structured objects.

    This type ensures that only structured updates (updates to complex objects)
    can be used, providing compile-time guarantees about the type of update.

    @param 'p The patch type for the structured object
*)
type 'p structured_update = ('p, structured) update

(** An update type specifically for atomic values.

    This type ensures that only atomic updates (updates to primitive values)
    can be used, providing compile-time type safety.

    @param 'a The type of the atomic value
*)
type 'a atomic_update = ('a atomic_patch, atomic) update


let diff_value ~equal ~diff old_value new_value =
  if equal old_value new_value then
    `Unchanged
  else
    `Modified (diff old_value new_value)

let diff_value_opt ~diff_some old_value new_value =
  match (old_value, new_value) with
  | (Some oldval, None) -> `Removed oldval
  | (None, Some newval) -> `Added newval
  | (Some oldval, Some newval) -> diff_some oldval newval
  | (None, None) -> `Unchanged

let diff_atomic_value (type a) (module EQ : EQUALABLE with type t = a)
    old_value new_value : a atomic_update =
  diff_value ~equal:EQ.equal ~diff:(fun oldval newval -> { oldval; newval }) old_value new_value

let diff_atomic_value_opt (type a) (module EQ : EQUALABLE with type t = a)
    (old_value : a option) (new_value : a option) : a atomic_change =
  diff_value_opt
    ~diff_some:(fun o n -> (diff_atomic_value (module EQ) o n :> a atomic_change))
    old_value new_value


let diff_complex_value (type a p)
    (module EQ : DIFFABLE_EQ with type t = a and type Patch.t = p)
    (old_value : a)
    (new_value : a) : p structured_update =
  diff_value ~equal:EQ.equal ~diff:EQ.diff old_value new_value

let diff_complex_value_id (type a p)
    (module ID : DIFFABLE_ID with type t = a and type Patch.t = p)
    (old_value : a)
    (new_value : a) : p structured_update =
  if ID.has_same_id old_value new_value then
    let patch = ID.diff old_value new_value in
    if ID.Patch.is_empty patch then `Unchanged
    else `Modified patch
  else
    failwith "diff_complex_value_id: IDs do not match"

let diff_complex_value_opt (type a p)
    (module EQ : DIFFABLE_EQ with type t = a and type Patch.t = p)
    (old_value : a option)
    (new_value : a option) : (a, p) structured_change =
  diff_value_opt
    ~diff_some:(fun o n -> (diff_complex_value (module EQ) o n :> (a, p) structured_change))
    old_value new_value


let diff_complex_value_id_opt (type a p)
    (module ID : DIFFABLE_ID with type t = a and type Patch.t = p)
    (old_value : a option)
    (new_value : a option) : (a, p) structured_change =
  diff_value_opt
    ~diff_some:(fun o n -> (diff_complex_value_id (module ID) o n :> (a, p) structured_change))
    old_value new_value


(** Check if a change or update represents no actual modification.

    This unified function works with both change and update types by using
    polymorphic variants to handle both cases safely.

    For structured changes:
    {[
      let unchanged = is_unchanged_change (module MyPatch) structured_change
    ]}

    For structured updates:
    {[
      let unchanged = is_unchanged_update (module MyPatch) structured_update
    ]}

    @param P A PATCH module for the patch type
    @return true if the operation represents no modification
*)
let is_unchanged_change (type a p)
    (module P : PATCH with type t = p)
    (operation : (a, p, structured) change) : bool =
  match operation with
  | `Added _ | `Removed _ -> false
  | `Unchanged -> true
  | `Modified p -> P.is_empty p

(** Check if an update represents no actual modification.

    This function specifically handles update types which lack Added/Removed variants.

    @param P A PATCH module for the patch type
    @param operation The update to check
    @return true if the update represents no modification
*)
let is_unchanged_update (type p)
    (module P : PATCH with type t = p)
    (operation : (p, structured) update) : bool =
  match operation with
  | `Unchanged -> true
  | `Modified p -> P.is_empty p


(** Check if an atomic change represents no actual modification.

    For atomic changes, the patch equality is checked using direct value comparison
    since atomic types don't have corresponding PATCH modules.

    @param c The atomic change to check
    @return true if the change represents no modification
*)
let is_unchanged_atomic_change (type a) (c : a atomic_change) : bool =
  is_unchanged_change
    (module struct
      type t = a atomic_patch
      let is_empty {oldval; newval} = oldval = newval
    end)
    c

(** Check if an atomic update represents no actual modification.

    For atomic updates, the patch equality is checked using direct value comparison
    since atomic types don't have corresponding PATCH modules.

    @param u The atomic update to check
    @return true if the update represents no modification
*)
let is_unchanged_atomic_update (type a) (u : a atomic_update) : bool =
  is_unchanged_update
    (module struct
      type t = a atomic_patch
      let is_empty {oldval; newval} = oldval = newval
    end)
    u


(* Module type for a hashable type, used by diff_set_generic *)
module type HASHER = sig
  type t
  val equal : t -> t -> bool
  val hash : t -> int
end


(** Generic Myers algorithm for ordered list diffing.

    @param compare Function to compare elements for equality
    @param on_match Function to generate change type for matching elements
    @param old_list Original list
    @param new_list Modified list
    @return List of flat changes representing the minimal edit sequence
*)
let diff_list_generic (type a p k)
    ~(compare: a -> a -> bool)
    ~(on_match: a -> a -> (a, p, k) change)
    (old_list : a list) (new_list : a list) : (a, p, k) change list =
  let old_arr = Array.of_list old_list in
  let new_arr = Array.of_list new_list in
  let n = Array.length old_arr in
  let m = Array.length new_arr in

  (* Handle edge cases *)
  if n = 0 then List.map (fun x -> `Added x) new_list
  else if m = 0 then List.map (fun x -> `Removed x) old_list
  else
    let fast_path_result =
      if n <> m then
        None
      else
        let all_match = ref true in
        let i = ref 0 in
        while !all_match && !i < n do
          if compare old_arr.(!i) new_arr.(!i) then
            incr i
          else
            all_match := false
        done;
        if !all_match then begin
          let result = ref [] in
          for idx = n - 1 downto 0 do
            result := on_match old_arr.(idx) new_arr.(idx) :: !result
          done;
          Some !result
        end else
          None
    in
    match fast_path_result with
    | Some result -> result
    | None ->
      (* Myers O(ND) algorithm implementation *)
      let max_d = n + m in
      let offset = max_d in (* Offset to handle negative k indices *)

      (* V array stores the furthest x position for each k-line *)
      let v = Array.make (2 * max_d + 1) 0 in
      (* Trace array stores V states for backtracking *)
      let traces = Array.init (max_d + 1) (fun _ -> Array.make (2 * max_d + 1) 0) in

      (* Follow diagonal (matching elements) as far as possible *)
      let follow_snake x y =
        let x_ref = ref x in
        let y_ref = ref y in
        while !x_ref < n && !y_ref < m && compare old_arr.(!x_ref) new_arr.(!y_ref) do
          incr x_ref;
          incr y_ref
        done;
        (!x_ref, !y_ref)
      in

      (* Forward search to find the shortest edit distance *)
      let search () =
        let found = ref None in
        let d = ref 0 in
        while !found = None && !d <= max_d do
          traces.(!d) <- Array.copy v;

          let found_at_d = ref false in
          let k = ref (- !d) in
          while not !found_at_d && !k <= !d do
            let x =
              if !k = - !d || (!k <> !d && v.(!k - 1 + offset) < v.(!k + 1 + offset)) then
                v.(!k + 1 + offset)
              else
                v.(!k - 1 + offset) + 1
            in
            let y = x - !k in
            let x_end, y_end = follow_snake x y in
            v.(!k + offset) <- x_end;
            if x_end >= n && y_end >= m then
              found_at_d := true
            else
              k := !k + 2
          done;

          if !found_at_d then
            found := Some !d
          else
            incr d
        done;
        match !found with
        | Some d -> d
        | None -> failwith "Myers algorithm: exceeded maximum edit distance"
      in

      (* Find the edit distance *)
      let edit_distance = search () in

      (* Backtrack to reconstruct the edit script *)
      let result = ref [] in

      let d_ref = ref edit_distance in
      let x_ref = ref n in
      let y_ref = ref m in
      while !d_ref > 0 do
        let d = !d_ref in
        let prev_v = traces.(d) in
        let k = !x_ref - !y_ref in

        let prev_k =
          if k = -d || (k <> d && prev_v.(k - 1 + offset) < prev_v.(k + 1 + offset)) then
            k + 1
          else
            k - 1
        in

        let prev_x = prev_v.(prev_k + offset) in
        let prev_y = prev_x - prev_k in
        let snake_start_x, snake_start_y =
          if prev_k = k - 1 then (prev_x + 1, prev_y)
          else (prev_x, prev_y + 1)
        in

        let curr_x = ref !x_ref in
        let curr_y = ref !y_ref in
        while !curr_x > snake_start_x && !curr_y > snake_start_y do
          result := on_match old_arr.(!curr_x - 1) new_arr.(!curr_y - 1) :: !result;
          decr curr_x;
          decr curr_y
        done;

        if prev_k = k - 1 then
          result := `Removed old_arr.(snake_start_x - 1) :: !result
        else
          result := `Added new_arr.(snake_start_y - 1) :: !result;

        x_ref := prev_x;
        y_ref := prev_y;
        decr d_ref
      done;

      for i = !x_ref - 1 downto 0 do
        result := on_match old_arr.(i) new_arr.(i) :: !result
      done;

      !result


(** Myers' O(ND) diff algorithm - based on Eugene W. Myers' 1986 paper.
    Returns a list of changes representing the shortest edit script.
    Time complexity: O((N+M)D) where D is the size of the edit script.
    Space complexity: O((N+M)D) for trace storage.
*)
let diff_list (type a p k) (module EQ : DIFFABLE_EQ with type t = a and type Patch.t = p) (old_list : a list) (new_list : a list) : (a, p, k) change list =
  diff_list_generic
    ~compare:EQ.equal
    ~on_match:(fun old_item new_item ->
        if EQ.equal old_item new_item then
          `Unchanged
        else
          `Modified (EQ.diff old_item new_item)
      )
    old_list new_list

let diff_list_id (type a p k) (module ID : DIFFABLE_ID with type t = a and type Patch.t = p) (old_list : a list) (new_list : a list) : (a, p, k) change list =
  diff_list_generic
    ~compare:ID.has_same_id
    ~on_match:(fun old_item new_item ->
        let patch = ID.diff old_item new_item in
        if ID.Patch.is_empty patch then
          `Unchanged
        else
          `Modified patch
      )
    old_list new_list

(* Utility functions *)
let update_of_patch (type a) (module P : PATCH with type t = a)
    (x : a) : a structured_update =
  if P.is_empty x then
    `Unchanged
  else
    `Modified x

let update_of_atomic (type a p)
    (module D : DIFFABLE_EQ with type t = a and type Patch.t = p)
    (x : a atomic_update) : p structured_update =
  match x with
  | `Modified { oldval; newval } -> `Modified (D.diff oldval newval)
  | `Unchanged -> `Unchanged

let structured_of_atomic (type a p)
    (module D : DIFFABLE_EQ with type t = a and type Patch.t = p)
    (x : a atomic_change) : (a, p) structured_change =
  match x with
  | `Added a -> `Added a
  | `Removed a -> `Removed a
  | `Unchanged -> `Unchanged
  | `Modified { oldval; newval } -> `Modified (D.diff oldval newval)


(** Post-process a change list to merge adjacent Removed+Added pairs into Modified.

    This enables replacement detection for anonymous sequences (without IDs) by
    converting patterns like [`Removed old; `Added new] into [`Modified patch].

    The merging only happens for immediately adjacent pairs. For example:
    - [`Removed 1; `Added 2; `Unchanged] becomes [`Modified {1,2}; `Unchanged]
    - [`Removed 1; `Unchanged; `Added 2] stays unchanged (not adjacent)

    @param diff Function to create a patch from old and new values
    @param changes The change list from Myers diff
    @return Change list with adjacent Removed+Added pairs merged into Modified
*)
let merge_adjacent_changes (type a p k)
    ~(diff : a -> a -> p)
    (changes : (a, p, k) change list) : (a, p, k) change list =
  let rec aux = function
    | `Removed old :: `Added new_ :: rest ->
      `Modified (diff old new_) :: aux rest
    | x :: rest -> x :: aux rest
    | [] -> []
  in
  aux changes


(** Convenience function combining diff_list with merge_adjacent_changes.

    This provides replacement detection for equality-based diffing by first
    computing the Myers diff, then merging adjacent Removed+Added pairs.

    Note: This may produce different results than diff_list for the same input,
    as adjacent insert+delete pairs are collapsed into modifications.
*)
let diff_list_merged (type a p k)
    (module EQ : DIFFABLE_EQ with type t = a and type Patch.t = p)
    (old_list : a list) (new_list : a list) : (a, p, k) change list =
  diff_list (module EQ) old_list new_list
  |> merge_adjacent_changes ~diff:EQ.diff


(** Filter out Unchanged entries from a change list.

    This removes `Unchanged` entries as well as `Modified` entries where
    the patch is empty (no actual changes).

    @param P A PATCH module for the patch type
    @param changes The change list to filter
    @return Change list with Unchanged entries removed
*)
let filter_changes (type a p)
    (module P : PATCH with type t = p)
    (changes : (a, p) structured_change list) : (a, p) structured_change list =
  List.filter (fun c -> not (is_unchanged_change (module P) c)) changes
