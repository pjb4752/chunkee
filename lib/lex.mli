exception SyntaxError of string * int * int

module Form : sig
  type meta = { line_num: int; char_num: int }

  type t =
    | Number of float * meta
    | String of string * meta
    | Symbol of string * meta
    | Cons of string * meta
    | List of t list * meta
    | Vec of t list * meta

  val to_string: t -> string

  val metadata: t -> meta

  val debug_string: t -> string
end

val lex: string -> (Form.t list, Cmpl_err.t) result
