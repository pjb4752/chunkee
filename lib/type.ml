open Thwack.Option
open Printf

type t =
  | Unit
  | Any
  | Num
  | Str
  | Bool
  | List
  | Fn of t list * t

let type_of_str s =
  if s = "unit" then Some Unit
  else if s = "any" then Some Any
  else if s = "num" then Some Num
  else if s = "str" then Some Str
  else if s = "bool" then Some Bool
  else if s = "list" then Some List
  else None

(*TODO add tests for this*)
let rec from_node = function
  | Node.TypeDef.StrType s -> type_of_str s
  | Node.TypeDef.FnType f ->
    let fold_fn ts t =
      (from_node t) >>= fun t ->
      ts >>= fun ts ->
      return (t :: ts) in
    match List.fold_left fold_fn (Some []) f with
    | Some [] -> assert false
    | Some (rt :: pt) -> Some (Fn (List.rev pt, rt))
    | None -> None

let rec to_string t =
  let string_of_fn pt rt =
    let types = List.append pt [rt] in
    let types = List.map to_string types in
    sprintf "[%s]" (String.concat " " types) in
  match t with
  | Unit -> "unit"
  | Any -> "any"
  | Num -> "num"
  | Str -> "str"
  | Bool -> "bool"
  | List -> "list"
  | Fn (pt, rt) -> string_of_fn pt rt
