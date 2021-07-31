open Printf

type t =
  | Unit
  | Any
  | Number
  | String
  | Bool
  | List
  | Vector
  | Record of (string * t) list
  | Function of t list * t

let type_of_str s =
  if s = "unit" then Some Unit
  else if s = "any" then Some Any
  else if s = "num" then Some Number
  else if s = "str" then Some String
  else if s = "bool" then Some Bool
  else if s = "list" then Some List
  else if s = "vector" then Some Vector
  else None

let find_builtin s = type_of_str s

let inspect_record inspect' fields =
  let fields = List.map (fun (_, field_type) -> inspect' field_type) fields in
  let fields = sprintf "[%s]" (String.concat "; " fields) in
  sprintf "Record({%s})" fields

let inspect_function inspect' param_types return_type =
  let types = List.append param_types [return_type] in
  let types = List.map inspect' types in
  sprintf "[%s]" (String.concat "; " types)

let rec inspect tipe =
  let inspect_record = inspect_record inspect in
  let inspect_function = inspect_function inspect in
  match tipe with
  | Unit -> "unit"
  | Any -> "any"
  | Number -> "num"
  | String -> "str"
  | Bool -> "bool"
  | List -> "list"
  | Vector -> "vector"
  | Record fields -> inspect_record fields
  | Function (param_types, return_type) -> inspect_function param_types return_type
