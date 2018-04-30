type t = Resolve.t * Type.t

val check_node: Table.t -> Module.t -> Resolve.t -> (Type.t, Cmpl_err.t) result

val check: Table.t -> Module.t -> Resolve.t list -> (t list, Cmpl_err.t) result
