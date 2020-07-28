type t =
  | BareName of string
  | QualName of Mod_name.t * string

val to_string: t -> string

val inspect: t -> string
