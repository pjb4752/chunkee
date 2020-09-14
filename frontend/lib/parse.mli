module Node = Ast.Parsed_node

type t = (Node.t, Cmpl_err.t) result

val parse_node: Lex.Form.t -> t

val inspect: t -> string
