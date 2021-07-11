type t

val make: string -> Type.t -> t

val name: t -> string

val tipe: t -> Type.t

val inspect: t -> string
