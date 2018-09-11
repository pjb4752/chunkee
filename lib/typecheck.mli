type node_t = Resolve.Resolve.node_t

type n = Resolve.Resolve.node_t * Type.t
type s = (Type.t, Cmpl_err.t) result
type t = (n list, Cmpl_err.t) result

val check_node: Symbol_table.t -> Module.t -> node_t -> s

val check: Symbol_table.t -> Module.t -> node_t list -> t
