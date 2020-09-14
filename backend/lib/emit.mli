val emit_require: Frontend.Module_name.t -> string

val emit_node: Name_gen.t -> Frontend.Ast.Resolved_node.t -> Lua_snippet.t

val emit_typed_node: (Frontend.Ast.Resolved_node.t * Frontend.Type.t) -> string

val emit: (Frontend.Ast.Resolved_node.t * Frontend.Type.t) list -> string list
