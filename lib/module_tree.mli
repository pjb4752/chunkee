module Lib_tree : sig
  module Children : Map.S with type key = Mod_name.Name.t

  type t =
    | Node of t Children.t
    | Leaf of Module.t
end

type t

val empty: t

val find_module: t -> Mod_name.t -> Module.t option

val insert_module: t -> Module.t -> t

val update_module: t -> Module.t -> t

val to_string: t -> string
