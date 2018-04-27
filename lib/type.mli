type t =
  | Unit
  | Num
  | Str
  | Bool
  | List
  | Fn of t list * t

val from_node: Node.VarDef.Type.t -> t option

val to_string: t -> string
