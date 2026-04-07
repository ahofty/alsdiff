open Ppxlib
module List = ListLabels

let chain_and ~loc (exprs : expression list) : expression =
  match exprs with
  | [] -> [%expr true]
  | [single] -> single
  | first :: rest ->
    List.fold_left rest ~init:first ~f:(fun acc expr ->
        [%expr [%e acc] && [%e expr]])

let chain_or ~loc (exprs : expression list) : expression =
  match exprs with
  | [] -> [%expr false]
  | [single] -> single
  | first :: rest ->
    List.fold_left rest ~init:first ~f:(fun acc expr ->
        [%expr [%e acc] || [%e expr]])
