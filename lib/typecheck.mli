module Node = Ast.Resolved_node

type n = Node.t * Type.t
type s = (Module.t * Type.t, Cmpl_err.t) result
type t = (Module.t * n list, Cmpl_err.t) result

val check_node: Symbol_table.t -> Module.t -> Node.t -> s

val check: Symbol_table.t -> Module.t -> Node.t list -> t
