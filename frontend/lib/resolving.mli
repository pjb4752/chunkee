module PNode = Ast.Parsed_node
module RNode = Ast.Resolved_node

module Result : sig
  type t = (RNode.t, Cmpl_err.t) result

  val inspect: t -> string
end

val resolve_node: Symbol_table.t -> PNode.t -> Result.t
