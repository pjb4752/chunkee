open Printf

type t =
  | Unit
  | Num
  | Str
  | Bool
  | List
  | Fn of t list * t

let from_string s =
  if s = "unit" then Some Unit
  else if s = "num" then Some Num
  else if s = "str" then Some Str
  else if s = "bool" then Some Bool
  else if s = "list" then Some List
  else None

let rec to_string t =
  let string_of_fn pt rt =
    let types = List.append pt [rt] in
    let types = List.map to_string types in
    sprintf "[%s]" (String.concat " " types) in
  match t with
  | Unit -> "Unit"
  | Num -> "Number"
  | Str -> "String"
  | Bool -> "Bool"
  | List -> "List"
  | Fn (pt, rt) -> string_of_fn pt rt
