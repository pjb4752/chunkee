module Node = Ast.Resolved_node

type t = (Symbol_table.t * Node.t list, Cmpl_err.t) result

val compile_module: Symbol_table.t -> string -> t
