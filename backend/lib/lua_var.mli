type t

val make_const: string -> Frontend.Type.t -> t

val make_fn: string -> Frontend.Type.t -> t

val make_simple_op: string -> string -> Frontend.Type.t -> t

val make_mapped_op: string -> string -> string -> Frontend.Type.t -> t

val name: t -> string

val compiler_name: t -> string

val tipe: t -> Frontend.Type.t

val compiler_operator: t -> string -> Lua_operator.t option
