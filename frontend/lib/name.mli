module Var : sig
  type t =
    | Local of string
    | Module of Module_name.t * Identifier.t

  val to_string: t -> string

  val inspect: t -> string
end
