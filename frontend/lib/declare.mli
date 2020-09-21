module Node = Ast.Parsed_node

module Result : sig
  type t = (Symbol_table.t, Cmpl_err.t) result

  val inspect: t -> string
end

val define_node: Symbol_table.t -> Node.t -> Result.t
