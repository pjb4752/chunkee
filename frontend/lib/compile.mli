type t = (Symbol_table.t * Ast.Resolved_node.t list, Cmpl_err.t) result

val compile_module: Symbol_table.t -> string -> t
