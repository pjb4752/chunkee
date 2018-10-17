module PNode = Ast.Parsed_node
module RNode = Ast.Resolved_node

type t = (RNode.t list, Cmpl_err.t) result

val resolve_node: Symbol_table.t -> PNode.t -> (RNode.t, Cmpl_err.t) result

val resolve: Symbol_table.t -> PNode.t list -> t
