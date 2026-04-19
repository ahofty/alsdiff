open Msg

let handle_key (ev : Mosaic.Event.key) : t option =
  let key_data = Mosaic.Event.Key.data ev in
  match key_data.Matrix.Input.Key.key with
  | Matrix.Input.Key.Char c ->
    (match Uchar.to_char c with
     | 'j' -> Some MoveDown
     | 'k' -> Some MoveUp
     | 'h' -> Some MoveLeft
     | 'l' -> Some MoveRight
     | 'a' -> Some ResolveOurs
     | 't' -> Some ResolveTheirs
     | 'b' -> Some ResolveBase
     | 'c' -> Some EnterCherryPick
     | 'A' -> Some ResolveAllOurs
     | 'T' -> Some ResolveAllTheirs
     | ' ' | '\r' -> Some ToggleExpand
     | 'w' -> Some Write
     | 'v' -> Some ToggleView
     | '?' -> Some ShowHelp
     | 'q' -> Some Quit
     | _ -> None)
  | Matrix.Input.Key.Up -> Some MoveUp
  | Matrix.Input.Key.Down -> Some MoveDown
  | Matrix.Input.Key.Left -> Some MoveLeft
  | Matrix.Input.Key.Right -> Some MoveRight
  | Matrix.Input.Key.Page_up -> Some PageUp
  | Matrix.Input.Key.Page_down -> Some PageDown
  | Matrix.Input.Key.Home -> Some MoveToStart
  | Matrix.Input.Key.End -> Some MoveToEnd
  | Matrix.Input.Key.Enter -> Some ToggleExpand
  | Matrix.Input.Key.Escape ->
    Some HideHelp
  | Matrix.Input.Key.Tab -> Some NextConflict
  | _ -> None
