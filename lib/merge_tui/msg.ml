type t =
  | MoveUp | MoveDown | PageUp | PageDown | MoveToStart | MoveToEnd
  | ToggleExpand | MoveLeft | MoveRight
  | NextConflict | PrevConflict
  | ResolveOurs | ResolveTheirs | ResolveBase
  | ResolveAllOurs | ResolveAllTheirs
  | Write | ShowHelp | HideHelp | Quit
  | Resize of int * int
