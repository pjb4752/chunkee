exception SyntaxError of int * int * string

module Form : sig
  type t = {
    metadata: Metadata.t;
    lexed: u
  }
  and u =
    | Number of float
    | String of string
    | Symbol of string
    | List of (t list)
    | Vector of (t list)
    | Extension of t

  val metadata: t -> Metadata.t

  val source: t -> string

  val inspect: t -> string
end

module Result : sig
  type t = (Form.t list, Cmpl_err.t) result

  val inspect: t -> string
end

val lex: string -> Result.t
