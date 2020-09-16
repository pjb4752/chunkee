type t

val make: string -> string -> t

val make_with_mapping: string -> string -> string -> t

val name: t -> string

val compiler_name: t -> string

val wrapper_function_name: t -> string
