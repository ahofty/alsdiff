open Msg

let char_to_msg_browser c =
  match c with
  | 'q' -> Some Quit
  | '?' -> Some ToggleHelp
  | 'j' -> Some MoveDown
  | 'k' -> Some MoveUp
  | 'n' -> Some MoveDown
  | 'p' -> Some MoveUp
  | _ -> None

let char_to_msg_diff c =
  match c with
  | 'q' -> Some BackToBrowser
  | 'd' -> Some CycleDetailMode
  | '/' -> Some StartSearch
  | 'f' -> Some (ToggleChangeFilter None)
  | '?' -> Some ToggleHelp
  | 's' -> Some ToggleStats
  | '[' -> Some NavBack
  | ']' -> Some NavForward
  | 'E' -> Some ShowExportSelector
  | 'z' -> Some EnterFocus
  | 'n' -> Some MoveDown
  | 'p' -> Some MoveUp
  | 'h' -> Some MoveLeft
  | 'j' -> Some MoveDown
  | 'k' -> Some MoveUp
  | 'l' -> Some MoveRight
  | ' ' -> Some ToggleExpand
  | _ -> None

let handle_key ~(mode : Model.mode) ~(search_mode : bool) ~(export_selector_active : bool)
    (ev : Mosaic.Event.key) : t option =
  let key_data = Mosaic.Event.Key.data ev in
  match key_data.Matrix.Input.Key.key with
  | Matrix.Input.Key.Char c ->
    if search_mode then
      Some (UpdateSearch (Uchar.to_char c |> String.make 1))
    else
      (match mode with
       | Model.Browser -> char_to_msg_browser (Uchar.to_char c)
       | Model.Diff -> char_to_msg_diff (Uchar.to_char c)
       | Model.Help | Model.Stats -> None)
  | Matrix.Input.Key.Up ->
    if export_selector_active then Some (MoveExportSelection (-1))
    else if search_mode then None else Some MoveUp
  | Matrix.Input.Key.Down ->
    if export_selector_active then Some (MoveExportSelection 1)
    else if search_mode then None else Some MoveDown
  | Matrix.Input.Key.Left -> if search_mode then None else Some MoveLeft
  | Matrix.Input.Key.Right -> if search_mode then None else Some MoveRight
  | Matrix.Input.Key.Enter ->
    if export_selector_active then Some ExecuteExport
    else if search_mode then Some EndSearch
    else
      (match mode with
       | Model.Browser -> Some BrowserActivate
       | Model.Diff -> Some ToggleExpand
       | Model.Help | Model.Stats -> None)
  | Matrix.Input.Key.Escape ->
    if export_selector_active then Some HideExportSelector
    else if search_mode then Some ClearSearch
    else
      (match mode with
       | Model.Browser -> Some BrowserGoUp
       | Model.Diff -> Some BackToBrowser
       | Model.Help -> Some HideHelp
       | Model.Stats -> Some HideStats)
  | Matrix.Input.Key.Backspace ->
    if search_mode then Some (UpdateSearch "\127") else None
  | Matrix.Input.Key.Page_up ->
    if export_selector_active then None
    else if search_mode then None else Some PageUp
  | Matrix.Input.Key.Page_down ->
    if export_selector_active then None
    else if search_mode then None else Some PageDown
  | Matrix.Input.Key.Home ->
    if export_selector_active then None
    else if search_mode then None else Some MoveToStart
  | Matrix.Input.Key.End ->
    if export_selector_active then None
    else if search_mode then None else Some MoveToEnd
  | _ -> None
