type t =
  | MoveUp | MoveDown | PageUp | PageDown | MoveToStart | MoveToEnd
  | ToggleExpand | MoveLeft | MoveRight
  | NextConflict | PrevConflict
  | ResolveOurs | ResolveTheirs | ResolveBase
  | ResolveAllOurs | ResolveAllTheirs
  | Write | ShowHelp | HideHelp | Quit
  | ToggleView
  | Resize of int * int
  | EnterCherryPick | LeaveCherryPick
  | SelectFieldOurs | SelectFieldTheirs | SelectFieldBase
  | CherryPickNextField | CherryPickPrevField
  | ApplyCherryPick
