open Thwack.Option
open Printf

module Name = Id

type t =
  | Unit
  | Any
  | Num
  | Str
  | Bool
  | List
  | Rec of Mod_name.t * Name.t
  | Fn of t list * t

let type_of_str s =
  if s = "unit" then Some Unit
  else if s = "any" then Some Any
  else if s = "num" then Some Num
  else if s = "str" then Some Str
  else if s = "bool" then Some Bool
  else if s = "list" then Some List
  else None

let find_builtin s = type_of_str s

let rec to_string t =
  let string_of_rec m_name name =
    sprintf "(rec %s/%s)" (Mod_name.to_string m_name) (Name.to_string name) in
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
  | Rec (m_name, name) -> string_of_rec m_name name
  | Fn (pt, rt) -> string_of_fn pt rt
