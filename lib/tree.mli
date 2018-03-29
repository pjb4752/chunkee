module Children : Map.S with type key = Module.Name.t

type t =
  | Node of t Children.t
  | Leaf of Module.t

val make: unit -> t

val find_module: t -> Children.key list -> Module.t option

val insert_module: t -> Children.key list -> Module.t -> t

val to_string: t -> string
