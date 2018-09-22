module Name = Id

module Path: sig
  type t

  val from_list: Name.t list -> t

  val to_list: t -> Name.t list
end

type t

val make: Path.t -> Name.t -> t

val short_name: t -> Name.t

val path_list: t -> Name.t list

val to_string: t -> string

val to_list: t -> Name.t list
