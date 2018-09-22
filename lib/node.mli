module Name = Id

module TypeDef: sig
  type t =
    | StrType of string
    | FnType of t list

  val from_string: string -> t

  val from_list: t list -> t

  val to_string: t -> string
end

module VarDef: sig
  module Name = Id

  type 'b t

  val from_parts: Name.t -> 'b -> 'b t

  val to_tuple: 'b t -> Name.t * 'b

  val to_string: ('b -> string) -> 'b t -> string
end

module Binding: sig
  module Name = Id
  type 'a t

  val from_node: Name.t -> 'a -> 'a t

  val to_string: 'a t -> ('a -> string) -> string

  val to_tuple: 'a t -> Name.t * 'a
end

type ('a, 'b) t =
  | NumLit of float
  | StrLit of string
  | SymLit of 'a
  | Rec of (Name.t * 'b VarDef.t list)
  | Def of (Name.t * ('a, 'b) t)
  | Fn of ('b VarDef.t list * ('a, 'b) t)
  | If of (('a, 'b) t * ('a, 'b) t * ('a, 'b) t)
  | Let of (('a, 'b) t Binding.t list * ('a, 'b) t)
  | Apply of (('a, 'b) t * ('a, 'b) t list)
  | Cast of ('b * ('a, 'b) t)

val to_string: ('a -> string) -> ('b -> string) -> ('a, 'b) t -> string
