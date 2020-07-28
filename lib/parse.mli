module Node = Ast.Parsed_node

type t = (Node.t list, Cmpl_err.t) result

val parse: Lex.Form.t list -> t

val inspect: t -> string
