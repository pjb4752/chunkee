module Node = Ast.Resolved_node

type t = (Type.t, Compile_error.t) result

val typecheck_node: Symbol_table.t -> Node.t -> t
