open Printf

type t =
  | Unit
  | Any
  | Num
  | Str
  | Bool
  | List
  | Rec of Mod_name.t * Identifier.t * rec_cons_t
  | Fn of t list * t
and rec_cons_t = (Identifier.t * t) list

let type_of_str s =
  if s = "unit" then Some Unit
  else if s = "any" then Some Any
  else if s = "num" then Some Num
  else if s = "str" then Some Str
  else if s = "bool" then Some Bool
  else if s = "list" then Some List
  else None

let find_builtin s = type_of_str s

let rec_to_string mod_name name =
  let mod_name = Mod_name.to_string mod_name in
  let name = Identifier.to_string name in
  sprintf "(rec %s/%s)" mod_name name

let fn_to_string to_string' param_types ret_type =
  let types = List.append param_types [ret_type] in
  let types = List.map to_string' types in
  sprintf "[%s]" (String.concat " " types)

let rec to_string tipe =
  let fn_to_string = fn_to_string to_string in
  match tipe with
  | Unit -> "unit"
  | Any -> "any"
  | Num -> "num"
  | Str -> "str"
  | Bool -> "bool"
  | List -> "list"
  | Rec (mod_name, name, _) -> rec_to_string mod_name name
  | Fn (param_types, ret_type) -> fn_to_string param_types ret_type

let inspect_rec inspect' module_name name fields =
  let module_name = Mod_name.inspect module_name in
  let name = Identifier.inspect name in
  let fields = List.map (fun (_, field_type) -> inspect' field_type) fields in
  let fields = sprintf "[%s]" (String.concat "; " fields) in
  sprintf "Rec(%s, %s, %s)" module_name name fields

let inspect_fn inspect' param_types return_type =
  let types = List.append param_types [return_type] in
  let types = List.map inspect' types in
  sprintf "[%s]" (String.concat "; " types)

let rec inspect tipe =
  let inspect_rec = inspect_rec inspect in
  let inspect_fn = inspect_fn inspect in
  match tipe with
  | Unit -> "Unit"
  | Any -> "Any"
  | Num -> "Num"
  | Str -> "Str"
  | Bool -> "Bool"
  | List -> "List"
  | Rec (module_name, name, fields) -> inspect_rec module_name name fields
  | Fn (param_types, return_type) -> inspect_fn param_types return_type
