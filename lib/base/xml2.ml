(** Streaming XML infrastructure over xmlm.

    Thin wrapper around [Xmlm.input] providing simplified signals with depth
    tracking. No DOM allocation — elements are delivered as [El_start]/[El_end]
    pairs so callers can process them incrementally. *)

type signal =
  | El_start of string * (string * string) list  (** Element name and attributes *)
  | El_end
  | Data of string

type stream = {
  xmlm_input : Xmlm.input;
  channel : In_channel.t option;  (* Keep channel alive to prevent GC closing FD *)
  mutable depth : int;
}

(** Strip namespace URIs from xmlm tags.
    Ableton XML doesn't use namespaces, so we discard the URI component. *)
let simplify_tag (tag : Xmlm.tag) =
  let (_, name) = fst tag in
  let attrs = List.map (fun a -> let ((_, k), v) = a in (k, v)) (snd tag) in
  (name, attrs)

let stream_from_input ?channel (i : Xmlm.input) =
  (* Consume the Dtd signal so callers never see it *)
  (match Xmlm.peek i with
   | `Dtd _ -> ignore (Xmlm.input i)
   | _ -> ());
  { xmlm_input = i; channel; depth = 0 }

let stream_from_string s =
  let i = Xmlm.make_input ~strip:true (`String (0, s)) in
  stream_from_input i

let stream_from_file filename =
  let ic = In_channel.open_text filename in
  let i = Xmlm.make_input ~strip:true (`Channel ic) in
  stream_from_input ~channel:ic i

let rec next_signal s =
  match Xmlm.input s.xmlm_input with
  | `El_start tag ->
    let (name, attrs) = simplify_tag tag in
    s.depth <- s.depth + 1;
    Some (El_start (name, attrs))
  | `El_end ->
    s.depth <- s.depth - 1;
    Some El_end
  | `Data d ->
    Some (Data d)
  | `Dtd _ ->
    (* Should have been consumed during init, but handle gracefully *)
    next_signal s
  | exception End_of_file ->
    None
  | exception (Xmlm.Error (_, `Unexpected_eoi)) ->
    None

let depth s = s.depth

let iter_signals f s =
  let rec loop () =
    match next_signal s with
    | Some sigv -> f sigv; loop ()
    | None -> ()
  in
  loop ()

let fold_signals f acc s =
  let rec loop acc =
    match next_signal s with
    | Some sigv -> loop (f sigv acc)
    | None -> acc
  in
  loop acc
