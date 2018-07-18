type node_t = Resolve.Resolve.node_t

type t = Resolve.Resolve.node_t * Type.t

val check_node: Symbol_table.t -> Module.t -> node_t -> (Type.t, Cmpl_err.t) result

val check: Symbol_table.t -> Module.t -> node_t list -> (t list, Cmpl_err.t) result
