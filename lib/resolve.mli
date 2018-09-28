module Define : sig
  module Node = Ast.Parsed_node

  type t = (Module.t, Cmpl_err.t) result

  val node_define: Module.t -> Node.t -> t

  val define: Module.t -> Node.t list -> t
end

module Resolve : sig
  module PNode = Ast.Parsed_node
  module RNode = Ast.Resolved_node

  type s = (RNode.t, Cmpl_err.t) result
  type t = (RNode.t list, Cmpl_err.t) result

  val resolve_node: Symbol_table.t -> Module.t -> PNode.t -> s

  val resolve_names: Symbol_table.t -> Module.t -> PNode.t list -> t

end
