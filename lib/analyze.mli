val analyze: Lex.Form.t -> (Node.t, Cmpl_err.t) result

val analyze_all: Lex.Form.t list -> (Node.t list, Cmpl_err.t) result
