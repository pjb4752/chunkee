(*
 * Represents a Lua operator.
 *
 * If operators map directly from the source language to Lua, like (=) then it can be emitted directly
 * into, else it can be created with a mapping value.
 *
 * A wrapper function name is also required since Lua cannot treat operators as function literals.
 *)

type t

val make: string -> string -> t

val make_with_mapping: string -> string -> string -> t

val name: t -> string

val compiler_name: t -> string

val wrapper_function_name: t -> string
