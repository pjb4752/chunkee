exception SyntaxError of string

module Form : sig
  type t =
    | Number of float
    | String of string
    | Symbol of string
    | List of t list

  val to_string: t -> string
end

val lex_exn: string -> Form.t list

val lex: string -> (Form.t list, Cmpl_err.t) result
