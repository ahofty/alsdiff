open Msg

let char_to_msg_browser c =
  match c with
  | 'q' -> Some Quit
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
  | 'n' -> Some MoveDown
  | 'p' -> Some MoveUp
  | 'h' -> Some MoveLeft
  | 'j' -> Some MoveDown
  | 'k' -> Some MoveUp
  | 'l' -> Some MoveRight
  | ' ' -> Some ToggleExpand
  | _ -> None

let handle_key ~(mode : Model.mode) ~(search_mode : bool) (ev : Mosaic.Event.key) : t option
  =
  let key_data = Mosaic.Event.Key.data ev in
  match key_data.Matrix.Input.Key.key with
  | Matrix.Input.Key.Char c ->
    if search_mode then
      Some (UpdateSearch (Uchar.to_char c |> String.make 1))
    else
      (match mode with
       | Model.Browser -> char_to_msg_browser (Uchar.to_char c)
       | Model.Diff -> char_to_msg_diff (Uchar.to_char c))
  | Matrix.Input.Key.Up -> if search_mode then None else Some MoveUp
  | Matrix.Input.Key.Down -> if search_mode then None else Some MoveDown
  | Matrix.Input.Key.Left -> if search_mode then None else Some MoveLeft
  | Matrix.Input.Key.Right -> if search_mode then None else Some MoveRight
  | Matrix.Input.Key.Enter ->
    if search_mode then Some EndSearch
    else
      (match mode with
       | Model.Browser -> Some BrowserActivate
       | Model.Diff -> Some ToggleExpand)
  | Matrix.Input.Key.Escape ->
    if search_mode then Some ClearSearch
    else
      (match mode with
       | Model.Browser -> Some BrowserGoUp
       | Model.Diff -> Some BackToBrowser)
  | Matrix.Input.Key.Backspace ->
    if search_mode then Some (UpdateSearch "\127") else None
  | Matrix.Input.Key.Page_up -> if search_mode then None else Some PageUp
  | Matrix.Input.Key.Page_down -> if search_mode then None else Some PageDown
  | Matrix.Input.Key.Home -> if search_mode then None else Some MoveToStart
  | Matrix.Input.Key.End -> if search_mode then None else Some MoveToEnd
  | _ -> None
