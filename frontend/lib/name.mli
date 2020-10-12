type t =
  | Local of string
  | Module of Module_name.t * Identifier.t

val inspect: t -> string
