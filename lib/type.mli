type t =
  | Unit
  | Any
  | Num
  | Str
  | Bool
  | List
  | Rec of Module_name.t * Identifier.t * rec_cons_t
  | Fn of t list * t
and rec_cons_t = (Identifier.t * t) list

val find_builtin: string -> t option

val to_string: t -> string

val inspect: t -> string
