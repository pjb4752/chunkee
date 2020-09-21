val emit_require: Frontend.Module_name.t -> string

val emit_lua_fragment: Name_gen.t -> Frontend.Ast.Resolved_node.t -> Lua_fragment.t

val emit_node_string: Frontend.Ast.Resolved_node.t -> string

val emit_node_strings: Frontend.Ast.Resolved_node.t list -> string list
