module Name = Id

type t =
  | Unit
  | Any
  | Num
  | Str
  | Bool
  | List
  | Rec of Mod_name.t * Name.t * rec_cons_t
  | Fn of t list * t
and rec_cons_t = (Name.t * t) list

val find_builtin: string -> t option

val to_string: t -> string

val inspect: t -> string
