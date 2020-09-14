module type InspectableType = sig
  type t

  val inspect: t -> string
end

module type N = sig
  type name_expr_t
  type type_expr_t

  module Binding: sig
    type 'a t

    val from_node: Identifier.t-> 'a -> 'a t

    val name: 'a t -> Identifier.t

    val expr: 'a t -> 'a

    val to_tuple: 'a t -> Identifier.t * 'a
  end

  module VarDef: sig
    type t

    val from_parts: Identifier.t -> type_expr_t -> t

    val to_tuple: t -> Identifier.t * type_expr_t
  end

  type t =
    | NumLit of float * Metadata.t
    | StrLit of string * Metadata.t
    | Symbol of name_expr_t * Metadata.t
    | Type of name_expr_t * Metadata.t
    | Rec of VarDef.t list * Metadata.t
    | Def of Identifier.t * t * Metadata.t
    | Fn of VarDef.t list * type_expr_t * t * Metadata.t
    | If of t * t * t * Metadata.t
    | Let of t Binding.t list * t * Metadata.t
    | Apply of t * t list * Metadata.t
    | Cons of type_expr_t * t Binding.t list * Metadata.t
    | Get of t * Identifier.t * Metadata.t
    | Set of t * Identifier.t * t * Metadata.t
    | Cast of type_expr_t * t * Metadata.t

  val inspect: t -> string
end

module Make (NameExpr: InspectableType) (TypeExpr: InspectableType) :
  N with type name_expr_t = NameExpr.t and type type_expr_t = TypeExpr.t
