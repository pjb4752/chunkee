type t =
  | BareName of string
  | QualName of Module_name.t * string

val inspect: t -> string
