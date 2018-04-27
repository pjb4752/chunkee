module VarDef: sig
  module Name = Id

  module Type: sig
    type t =
      | StrType of string
      | FnType of t list

    val from_string: string -> t

    val from_list: t list -> t

    val to_string: t -> string
  end

  type t

  val from_parts: Name.t -> Type.t -> t

  val to_tuple: t -> Name.t * Type.t

  val to_string: t -> string
end

module Binding: sig
  module Name = Id
  type 'a t

  val from_node: Name.t -> 'a -> 'a t

  val to_string: 'a t -> ('a -> string) -> string

  val values: 'a t -> Name.t * 'a
end

type 'a t =
  | NumLit of float
  | StrLit of string
  | SymLit of 'a
  | Def of (VarDef.t * 'a t)
  | Fn of (VarDef.t list * 'a t)
  | If of ('a t * 'a t * 'a t)
  | Let of ('a t Binding.t list * 'a t)
  | Apply of ('a t * 'a t list)

val to_string: ('a -> string) -> 'a t -> string
