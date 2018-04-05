type t =
  | Literal of string
  | Local of string
  | Module of Module.Var.Name.t

val to_string: t -> string
