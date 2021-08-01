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

  type t = private {
    position: Stream_position.t;
    parsed: u
  } and u = private
    | Number of float
    | String of string
    | Symbol of name_t
    | Def of { name: string; body_form: t }
    | Fn of { parameters: VarDef.t list; return_type: type_t; body_form: t }
    | If of { test_form: t; if_form: t; else_form: t }
    | Let of { bindings: t Binding.t list; body_form: t }
    | Apply of { callable_form: t; arguments: t list }
    | Cons of { target_type: type_t; bindings: t Binding.t list }
    | Get of { target_form: t; field: string }
    | Set of { target_form: t; field: string; body_form: t }
    | Cast of { target_type: type_t; body_form: t }

  val create_number: Stream_position.t -> float -> t
  val create_string: Stream_position.t -> string -> t
  val create_symbol: Stream_position.t -> name_t -> t
  val create_def: Stream_position.t -> string -> t -> t
  val create_fn: Stream_position.t -> VarDef.t list -> type_t -> t -> t
  val create_if: Stream_position.t -> t -> t -> t -> t
  val create_let: Stream_position.t -> t Binding.t list -> t -> t
  val create_apply: Stream_position.t -> t -> t list -> t
  val create_cons: Stream_position.t -> type_t -> t Binding.t list -> t
  val create_get: Stream_position.t -> t -> string -> t
  val create_set: Stream_position.t -> t -> string -> t -> t
  val create_cast: Stream_position.t -> type_t -> t -> t

  val inspect: t -> string
end

module Make (NameExpression: InspectableType) (TypeExpression: InspectableType) :
  N with type name_t = NameExpression.t and type type_t = TypeExpression.t
