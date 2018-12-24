module type ShowableType = sig
  type t

  val to_string: t -> string
end

module type N = sig

  type name_expr_t

  type type_expr_t

  module Name = Id

  module Binding: sig

    module Name = Id

    type 'a t

    val from_node: Name.t -> 'a -> 'a t

    val name: 'a t -> Name.t

    val expr: 'a t -> 'a

    val to_tuple: 'a t -> Name.t * 'a

    val to_string: ('a -> string) -> 'a t -> string
  end

  module VarDef: sig

    module Name = Id

    type t

    val from_parts: Name.t -> type_expr_t -> t

    val to_tuple: t -> Name.t * type_expr_t

    val to_string: t -> string
  end

  type t =
    | NumLit of float * Metadata.t
    | StrLit of string * Metadata.t
    | SymLit of name_expr_t * Metadata.t
    | Rec of Name.t * VarDef.t list * Metadata.t
    | Def of Name.t * t * Metadata.t
    | Fn of VarDef.t list * type_expr_t * t * Metadata.t
    | If of t * t * t * Metadata.t
    | Let of t Binding.t list * t * Metadata.t
    | Apply of t * t list * Metadata.t
    | Cons of type_expr_t * t Binding.t list * Metadata.t
    | Get of t * Name.t * Metadata.t
    | Set of t * Name.t * t * Metadata.t
    | Cast of type_expr_t * t * Metadata.t

  val to_string: t -> string
end

module Make (NameExpr: ShowableType) (TypeExpr: ShowableType) :
  N with type name_expr_t = NameExpr.t and type type_expr_t = TypeExpr.t
