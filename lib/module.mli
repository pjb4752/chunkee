module Name = Id

module Path: sig
  type t

  val from_list: Name.t list -> t

  val to_list: t -> Name.t list
end

module Qual_name: sig
  type t

  val make: Path.t -> Name.t -> t

  val to_string: t -> string

  val to_list: t -> Name.t list
end

module Var: sig
  module Name = Id
  type t

  val from_tuple: Qual_name.t * Name.t -> t

  val to_tuple: t -> Qual_name.t * Name.t

  val to_string: t -> string
end

type t

val from_name: Qual_name.t -> t

val from_parts: Path.t -> Name.t -> t

val short_name: t -> Name.t

val qual_name: t -> Qual_name.t

val path_list: t -> Name.t list

val qual_name_list: t -> Name.t list

val find_var: t -> Var.Name.t -> Var.t option

val var_exists: t -> Var.Name.t -> bool

val add_var: t -> Var.t -> t

val make_var: t -> Var.Name.t -> t

val to_string: t -> string
