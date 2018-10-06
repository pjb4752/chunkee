module PNode = Ast.Parsed_node
module RNode = Ast.Resolved_node

type r_node_rt = (RNode.t, Cmpl_err.t) result

type ts = (Module.t * RNode.t list, Cmpl_err.t) result

val resolve_node: Symbol_table.t -> Module.t -> PNode.t -> r_node_rt

val resolve: Symbol_table.t -> Module.t -> PNode.t list -> ts
