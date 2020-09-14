type t =
  | Unit
  | Any
  | Number
  | String
  | Bool
  | List
  | Record of (Identifier.t * t) list
  | Function of t list * t

val find_builtin: string -> t option

val inspect: t -> string
