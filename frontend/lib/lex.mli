exception SyntaxError of int * int * string

module Form : sig
  type t =
    | Number of Metadata.t * string * float
    | String of Metadata.t * string * string
    | Symbol of Metadata.t * string * string
    | List of Metadata.t * string * t list
    | Vector of Metadata.t * string * t list
    | Extension of Metadata.t * string * t

  val metadata: t -> Metadata.t

  val raw_source: t -> string

  val inspect: t -> string
end

module Result : sig
  type t = (Form.t list, Cmpl_err.t) result

  val inspect: t -> string
end

val lex: string -> Result.t
