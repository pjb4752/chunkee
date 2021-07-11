module type InspectableType = sig
  type t

  val inspect: t -> string
end

module type N = sig
  type name_t
  type type_t

  module Binding: sig
    type 'a t

    val from_form: string -> 'a -> 'a t

    val name: 'a t -> string

    val expr: 'a t -> 'a

    val to_tuple: 'a t -> string * 'a
  end

  module VarDef: sig
    type t

    val from_parts: string -> type_t -> t

    val to_tuple: t -> string * type_t
  end

  type t = {
    metadata: Metadata.t;
    parsed: u
  } and u =
    | Number of float
    | String of string
    | Symbol of name_t
    | Type of name_t
    | Def of { name: string; body_form: t }
    | Fn of { parameters: VarDef.t list; return_type: type_t; body_form: t }
    | If of { test_form: t; if_form: t; else_form: t }
    | Let of { bindings: t Binding.t list; body_form: t }
    | Apply of { callable_form: t; arguments: t list }
    | Cons of { target_type: type_t; bindings: t Binding.t list }
    | Get of { target_form: t; field: string }
    | Set of { target_form: t; field: string; body_form: t }
    | Cast of { target_type: type_t; body_form: t }

  val inspect: t -> string
end

module Make (NameExpression: InspectableType) (TypeExpression: InspectableType) :
  N with type name_t = NameExpression.t and type type_t = TypeExpression.t
