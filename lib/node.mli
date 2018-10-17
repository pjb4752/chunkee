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

    val to_string: ('a -> string) -> 'a t -> string

    val to_tuple: 'a t -> Name.t * 'a
  end

  module VarDef: sig

    module Name = Id

    type t

    val from_parts: Name.t -> type_expr_t -> t

    val to_tuple: t -> Name.t * type_expr_t

    val to_string: t -> string
  end

  type t =
    | NumLit of float
    | StrLit of string
    | SymLit of name_expr_t
    | Rec of Name.t * VarDef.t list
    | Def of Name.t * t
    | Fn of VarDef.t list * type_expr_t * t
    | If of t * t * t
    | Let of t Binding.t list * t
    | Apply of t * t list
    | Cast of type_expr_t * t

  val to_string: t -> string
end

module Make (NameExpr: ShowableType) (TypeExpr: ShowableType) :
  N with type name_expr_t = NameExpr.t and type type_expr_t = TypeExpr.t
