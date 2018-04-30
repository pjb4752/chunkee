type t =
  | Local of string
  | Module of Module.Qual_name.t * Module.Var.Name.t

val to_string: t -> string
