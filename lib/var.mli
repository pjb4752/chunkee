module Name = Id

type t

val make: Name.t -> Type.t -> t

val name: t -> Name.t

val tipe: t -> Type.t

val to_string: t -> string

val inspect: t -> string
