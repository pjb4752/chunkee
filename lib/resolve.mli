type t = Name.t Node.t

val define_var: Module.t -> Parse.t -> (Module.t, Cmpl_err.t) result

val define_vars: Module.t -> Parse.t list -> (Module.t, Cmpl_err.t) result

val resolve_node: Table.t -> Module.t -> Parse.t -> (t, Cmpl_err.t) result

val resolve: Table.t -> Module.t -> Parse.t list -> (t list, Cmpl_err.t) result
