type t =
  | SimpleType of Name_expr.t
  | CompoundType of t list

val inspect: t -> string
