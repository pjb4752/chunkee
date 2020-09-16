(*
 * An interface for a module written in Lua.
 *
 * Requires the name the module is given in the compiler, and the list of definitions exposed by the module.
 *)

type t

val make: Frontend.Module_name.t -> Lua_definition.t list -> t

val find_operator: t -> string -> Lua_operator.t option

val operator_exists: t -> string -> bool

val compiler_name: t -> Frontend.Module_name.t

val to_compiler_module: t -> Frontend.Module.t
