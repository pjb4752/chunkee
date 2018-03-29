module Name = Id

module Var: sig
  module Name = Id
  type t

  val make: Name.t -> t

  val to_string: t -> string
end

type t

val make: Name.t -> t

val name: t -> Name.t

val find_var: t -> Var.Name.t -> Var.t option

val var_exists: t -> Var.Name.t -> bool

val add_var: t -> Var.t -> t

val to_string: t -> string
