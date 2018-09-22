module V = Var
module T = Type

module Var : sig
  type t =
    | Local of string
    | Module of Mod_name.t * V.Name.t

  val to_string: t -> string
end

module Type : sig
  type t =
    | Builtin of string
    | UserDef of Mod_name.t * T.Name.t
end
