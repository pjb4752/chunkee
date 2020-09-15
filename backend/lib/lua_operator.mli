type t

val make_simple: string -> string -> t

val make_mapped: string -> string -> string -> t

val lua_name: t -> string

val compiler_name: t -> string

val wrapper_name: t -> string
