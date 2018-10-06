module Name = Id

type t =
  | Unit
  | Any
  | Num
  | Str
  | Bool
  | List
  | Rec of Mod_name.t * Name.t
  | Fn of t list * t

val find_builtin: string -> t option

val to_string: t -> string
