type compiler_module_t = Frontend.Module.t

type t

val make: Frontend.Module_name.t -> Lua_var.t list -> t

val name: t -> Frontend.Module_name.t

val find_operator: t -> string -> Lua_operator.t option

val operator_exists: t -> string -> bool

val to_compiler_module: t -> compiler_module_t
