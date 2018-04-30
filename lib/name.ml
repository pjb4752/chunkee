open Printf

type t =
  | Local of string
  | Module of Module.Qual_name.t * Module.Var.Name.t

let to_string = function
  | Local s -> sprintf "(local %s)" s
  | Module (qn, vn) -> sprintf "(module %s/%s)"
    (Module.Qual_name.to_string qn) (Module.Var.Name.to_string vn)
