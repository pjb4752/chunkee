module Name = Id

module Path: sig
  type t

  val from_list: Name.t list -> t

  val to_list: t -> Name.t list
end

type t

module Var: sig
  module Name = Id
  type t

  val make: Name.t -> t

  val to_string: t -> string
end

val make: Path.t -> Name.t -> t

val name: t -> Name.t

val full_path: t -> Path.t

val path_list: t -> Name.t list

val find_var: t -> Var.Name.t -> Var.t option

val var_exists: t -> Var.Name.t -> bool

val add_var: t -> Var.t -> t

val to_string: t -> string
