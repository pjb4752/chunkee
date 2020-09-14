open Printf

type t =
  | Unit
  | Any
  | Number
  | String
  | Bool
  | List
  | Record of (Identifier.t * t) list
  | Function of t list * t

let type_of_str s =
  if s = "Unit" then Some Unit
  else if s = "Any" then Some Any
  else if s = "Num" then Some Number
  else if s = "Str" then Some String
  else if s = "Bool" then Some Bool
  else if s = "List" then Some List
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
  | Unit -> "Unit"
  | Any -> "Any"
  | Number -> "Num"
  | String -> "Str"
  | Bool -> "Bool"
  | List -> "List"
  | Record fields -> inspect_record fields
  | Function (param_types, return_type) -> inspect_function param_types return_type
