module Node = Ast.Resolved_node

type t = ((Node.t * Type.t) list, Cmpl_err.t) result

type u = (Type.t, Cmpl_err.t) result

val check_node: Symbol_table.t -> Node.t -> u

val check: Symbol_table.t -> Node.t list -> t
