type t

val make_expr: string -> t

val make_unit_stmt: string -> t

val make_result_stmt: string -> string -> t

val insert_preamble: t -> t -> t

val is_expr: t -> bool

val is_stmt: t -> bool

val preamble_string: t -> string

val result_expr: t -> string

val lua_string: ?target:string -> t -> string

val to_string: t -> string
