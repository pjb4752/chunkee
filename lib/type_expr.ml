open Printf

type t =
  | SimpleType of Name_expr.t
  | FnType of t list

let simple_type_to_string tipe =
  Name_expr.to_string tipe

let fn_type_to_string to_string' types =
  let types = List.map to_string' types in
  sprintf "[%s]" (String.concat " " types)

let rec to_string = function
  | SimpleType tipe -> simple_type_to_string tipe
  | FnType types -> fn_type_to_string to_string types
