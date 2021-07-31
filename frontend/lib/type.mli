type t =
  | Unit
  | Any
  | Number
  | String
  | Bool
  | List
  | Vector
  | Record of (string * t) list
  | Function of t list * t

val find_builtin: string -> t option

val inspect: t -> string
