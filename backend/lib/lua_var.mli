type t

val make_constant: string -> Frontend.Type.t -> t

val make_function: string -> Frontend.Type.t -> t

val make_simple_operator: string -> string -> Frontend.Type.t -> t

val make_mapped_operator: string -> string -> string -> Frontend.Type.t -> t

val lua_name: t -> string

val compiler_name: t -> string

val compiler_type: t -> Frontend.Type.t

val lua_operator: t -> string -> Lua_operator.t option
