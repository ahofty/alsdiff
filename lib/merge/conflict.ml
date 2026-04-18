type resolution = Ours | Theirs | Base

type conflict_kind =
  | Atomic_conflict
  | Add_remove_conflict
  | Overlapping_modification

type t = {
  path : string;
  kind : conflict_kind;
  ours_desc : string;
  theirs_desc : string;
}

let kind_to_string = function
  | Atomic_conflict -> "atomic conflict"
  | Add_remove_conflict -> "add/remove conflict"
  | Overlapping_modification -> "overlapping modification"

let pp fmt { path; kind; ours_desc; theirs_desc } =
  Fmt.pf fmt "CONFLICT at %s (%s): ours=%s theirs=%s"
    path (kind_to_string kind) ours_desc theirs_desc

let resolution_to_string = function
  | Ours -> "ours"
  | Theirs -> "theirs"
  | Base -> "base"
