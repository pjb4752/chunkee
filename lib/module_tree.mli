module Lib_tree : sig
  module Children : Map.S with type key = Module_name.Segment.t

  type t =
    | Node of t Children.t
    | Leaf of Module.t
end

type t

val empty: t

val find_module: t -> Module_name.t -> Module.t option

val insert_module: t -> Module.t -> t

val update_module: t -> Module.t -> t

val to_string: t -> string
