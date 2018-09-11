module Name = Id

type t =
  | Unit
  | Any
  | Num
  | Str
  | Bool
  | List
  | Rec of Name.t
  | Fn of t list * t

val from_node: Node.TypeDef.t -> t option

val has_name: t -> Name.t -> bool

val to_string: t -> string
