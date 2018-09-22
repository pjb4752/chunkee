module V = Var

module Var : sig
  type t =
    | Local of string
    | Module of Mod_name.t * V.Name.t

  val to_string: t -> string
end
