val emit_require: Frontend.Module_name.t -> string

val emit_lua_fragment: Name_gen.t -> Frontend.Ast.Resolved_form.t -> Lua_fragment.t

val emit_form_string: Frontend.Ast.Resolved_form.t -> string

val emit_form_strings: Frontend.Ast.Resolved_form.t list -> string list
