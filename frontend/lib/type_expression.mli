type t =
  | SimpleType of Names.Unresolved_name.t
  | CompoundType of t list

val inspect: t -> string
