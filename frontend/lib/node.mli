module type InspectableType = sig
  type t

  val inspect: t -> string
end

module type N = sig
  type name_expr_t
  type type_expr_t

  module Binding: sig
    type 'a t

    val from_node: Identifier.t -> 'a -> 'a t

    val name: 'a t -> Identifier.t

    val expr: 'a t -> 'a

    val to_tuple: 'a t -> Identifier.t * 'a
  end

  module VarDef: sig
    type t

    val from_parts: Identifier.t -> type_expr_t -> t

    val to_tuple: t -> Identifier.t * type_expr_t
  end

  type t = {
    metadata: Metadata.t;
    parsed: u
  } and u =
    | NumLit of float
    | StrLit of string
    | Symbol of name_expr_t
    | Type of name_expr_t
    | Def of { name: Identifier.t; body_node: t }
    | Fn of { parameters: VarDef.t list; return_type: type_expr_t; body_node: t }
    | If of { test_node: t; if_node: t; else_node: t }
    | Let of { bindings: t Binding.t list; body_node: t }
    | Apply of { callable_node: t; arguments: t list }
    | Cons of { target_type: type_expr_t; bindings: t Binding.t list }
    | Get of { target_node: t; field: Identifier.t }
    | Set of { target_node: t; field: Identifier.t; body_node: t }
    | Cast of { target_type: type_expr_t; body_node: t }

  val inspect: t -> string
end

module Make (NameExpr: InspectableType) (TypeExpr: InspectableType) :
  N with type name_expr_t = NameExpr.t and type type_expr_t = TypeExpr.t
