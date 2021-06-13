module Result : sig
  type t = (Symbol_table.t, Cmpl_err.t) result

  val inspect: t -> string
end

val define_for_node: Symbol_table.t -> Ast.Parsed_node.t -> Result.t
