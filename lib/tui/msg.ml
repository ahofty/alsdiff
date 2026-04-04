type t =
  | MoveUp
  | MoveDown
  | MoveLeft
  | MoveRight
  | ToggleExpand
  | CycleDetailMode
  | StartSearch
  | UpdateSearch of string
  | ClearSearch
  | EndSearch
  | ToggleChangeFilter of Alsdiff_output.View_model.change_type option
  | JumpToPath of string list
  | Resize of int * int
  | Quit
  | BrowserActivate
  | BrowserGoUp
  | BackToBrowser
  | PageUp
  | PageDown
  | MoveToStart
  | MoveToEnd
