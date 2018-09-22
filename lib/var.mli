module Name = Id

type t

val declare: Name.t -> t

val define: Name.t -> Type.t -> t

val name: t -> Name.t

val tipe: t -> Type.t option

val to_string: t -> string
