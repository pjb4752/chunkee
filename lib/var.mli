type t

val make: Identifier.t -> Type.t -> t

val name: t -> Identifier.t

val tipe: t -> Type.t

val to_string: t -> string

val inspect: t -> string
