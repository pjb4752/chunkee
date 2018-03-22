module Param = Id

module Binding: sig
  module Name = Id
  type 'a t

  val from_node: Name.t -> 'a -> 'a t
end

type t =
  | NumLit of float
  | StrLit of string
  | SymLit of string
  | Def of (Module.Var.Name.t * t)
  | Fn of (Param.t list * t)
  | If of (t * t * t)
  | Let of (t Binding.t list * t)
  | Apply of (t * t list)

val to_string: t -> string
