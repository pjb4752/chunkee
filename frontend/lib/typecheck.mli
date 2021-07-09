type t = (Type.t, Compile_error.t) result

val typecheck_form: Symbol_table.t -> Ast.Resolved_form.t -> t
