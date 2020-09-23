module Node = Ast.Parsed_node

module Result : sig
  type t = (Node.t, Cmpl_err.t) result

  val inspect: t -> string
end

val parse_node: Lex.Form.t -> Result.t