open Msg

let char_to_msg c =
  match c with
  | 'q' -> Some Quit
  | 'd' -> Some CycleDetailMode
  | '/' -> Some StartSearch
  | 'f' -> Some (ToggleChangeFilter None)  (* update.ml cycles the filter *)
  | 'n' -> Some MoveDown
  | 'p' -> Some MoveUp
  | 'h' -> Some MoveLeft
  | 'j' -> Some MoveDown
  | 'k' -> Some MoveUp
  | 'l' -> Some MoveRight
  | ' ' -> Some ToggleExpand
  | '\027' -> Some ClearSearch (* Escape *)
  | _ -> None

let handle_key ~(search_mode:bool) (ev : Mosaic.Event.key) : t option =
  let key_data = Mosaic.Event.Key.data ev in
  match key_data.Matrix.Input.Key.key with
  | Matrix.Input.Key.Char c ->
    if search_mode then
      Some (UpdateSearch (Uchar.to_char c |> String.make 1))
    else
      char_to_msg (Uchar.to_char c)
  | Matrix.Input.Key.Up -> if search_mode then None else Some MoveUp
  | Matrix.Input.Key.Down -> if search_mode then None else Some MoveDown
  | Matrix.Input.Key.Left -> if search_mode then None else Some MoveLeft
  | Matrix.Input.Key.Right -> if search_mode then None else Some MoveRight
  | Matrix.Input.Key.Enter ->
    if search_mode then Some EndSearch else Some ToggleExpand
  | Matrix.Input.Key.Escape -> Some ClearSearch
  | Matrix.Input.Key.Backspace ->
    if search_mode then Some (UpdateSearch "\127") else None
  | _ -> None
