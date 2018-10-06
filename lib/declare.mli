module Node = Ast.Parsed_node

type t = (Module.t, Cmpl_err.t) result

val declare_toplevel: Module.t -> Node.t -> t

val declare_toplevels: Module.t -> Node.t list -> t
