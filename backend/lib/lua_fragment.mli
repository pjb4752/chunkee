(*
 * A Lua source fragment.
 *
 * Three forms exist:
 *   expression - a Lua fragment that is an expression
 *   result statement - a Lua fragment that is a statement, but must capture its result as an expession
 *   unit statement - a Lua fragment that does not have to capture its result
 *)

type t

val make_expression: string -> t

val make_unit_statement: string -> t

val make_result_statement: string -> string -> t

val insert_preamble: t -> t -> t

val is_expression: t -> bool

val is_statement: t -> bool

val preamble_string: t -> string

val result_expression: t -> string

val lua_string: ?target:string -> t -> string

val inspect: t -> string
