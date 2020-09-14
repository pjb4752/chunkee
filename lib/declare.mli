module Node = Ast.Parsed_node

type t = (Symbol_table.t, Cmpl_err.t) result

val declare_node: Symbol_table.t -> Node.t -> t
