type t =
  | Unit
  | Any
  | Num
  | Str
  | Bool
  | List
  | Fn of t list * t

val from_node: Node.TypeDef.t -> t option

val to_string: t -> string
