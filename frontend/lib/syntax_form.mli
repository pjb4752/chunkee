module type InspectableType = sig
  type t

  val inspect: t -> string
end

module type N = sig
  type name_expr_t
  type type_expr_t

  module Binding: sig
    type 'a t

    val from_form: string -> 'a -> 'a t

    val name: 'a t -> string

    val expr: 'a t -> 'a

    val to_tuple: 'a t -> string * 'a
  end

  module VarDef: sig
    type t

    val from_parts: string -> type_expr_t -> t

    val to_tuple: t -> string * type_expr_t
  end

  type t = {
    metadata: Metadata.t;
    parsed: u
  } and u =
    | NumLit of float
    | StrLit of string
    | Symbol of name_expr_t
    | Type of name_expr_t
    | Def of { name: string; body_form: t }
    | Fn of { parameters: VarDef.t list; return_type: type_expr_t; body_form: t }
    | If of { test_form: t; if_form: t; else_form: t }
    | Let of { bindings: t Binding.t list; body_form: t }
    | Apply of { callable_form: t; arguments: t list }
    | Cons of { target_type: type_expr_t; bindings: t Binding.t list }
    | Get of { target_form: t; field: string }
    | Set of { target_form: t; field: string; body_form: t }
    | Cast of { target_type: type_expr_t; body_form: t }

  val inspect: t -> string
end

module Make (NameExpr: InspectableType) (TypeExpr: InspectableType) :
  N with type name_expr_t = NameExpr.t and type type_expr_t = TypeExpr.t
