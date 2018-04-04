open Extensions

type t = string Node.t

val parse_form: Lex.Form.t -> (t, Cmpl_err.t) result

val parse: Lex.Form.t list -> (t list, Cmpl_err.t) result
