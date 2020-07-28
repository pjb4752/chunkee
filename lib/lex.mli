exception SyntaxError of string * int * int

module Form : sig
  type t =
    | Number of float * Metadata.t
    | String of string * Metadata.t
    | Symbol of string * Metadata.t
    | Cons of string * Metadata.t
    | List of t list * Metadata.t
    | Vec of t list * Metadata.t

  val to_string: t -> string

  val metadata: t -> Metadata.t

  val inspect: t -> string
end

type t = (Form.t list, Cmpl_err.t) result

val lex: string -> t

val inspect: t -> string
