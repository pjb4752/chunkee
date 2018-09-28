module Node = Ast.Parsed_node

val parse_form: Lex.Form.t -> (Node.t, Cmpl_err.t) result

val parse: Lex.Form.t list -> (Node.t list, Cmpl_err.t) result
