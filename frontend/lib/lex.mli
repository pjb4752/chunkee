exception SyntaxError of int * int * string

module Form : sig
  type t =
    | Number of Metadata.t * float
    | String of Metadata.t * string
    | Symbol of Metadata.t * string
    | List of Metadata.t * t list
    | Vector of Metadata.t * t list
    | Record of Metadata.t * t list
    | Extension of Metadata.t * t

  val metadata: t -> Metadata.t

  val to_string: t -> string

  val inspect: t -> string
end

type t = (Form.t list, Cmpl_err.t) result

val lex: string -> t

val inspect: t -> string
