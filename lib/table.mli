module Children : Map.S with type key = Module.Name.t

type t =
  | Node of t Children.t
  | Leaf of Module.t

val find_module: t -> Module.Path.t -> Module.t option

val insert_module: t -> Module.t -> t

val update_module: t -> Module.t -> t

val with_stdlib: t

val to_string: t -> string
