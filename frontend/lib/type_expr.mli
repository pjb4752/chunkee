type t =
  | SimpleType of Name_expr.t
  | FnType of t list

val to_string: t -> string

val inspect: t -> string
