module Param = Id

module Binding: sig
  module Name = Id
  type 'a t

  val from_node: Name.t -> 'a -> 'a t

  val to_string: 'a t -> ('a -> string) -> string
end

type 'a t =
  | NumLit of float
  | StrLit of string
  | SymLit of 'a
  | Def of (Module.Var.Name.t * 'a t)
  | Fn of (Param.t list * 'a t)
  | If of ('a t * 'a t * 'a t)
  | Let of ('a t Binding.t list * 'a t)
  | Apply of ('a t * 'a t list)

val to_string: ('a -> string) -> 'a t -> string
