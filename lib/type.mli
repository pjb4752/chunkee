type t =
  | Unit
  | Num
  | Str
  | Bool
  | List
  | Fn of t list * t

val from_string: string -> t option

val to_string: t -> string
