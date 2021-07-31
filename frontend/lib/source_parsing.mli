module Result: sig
  type t = (Source_form.t list, Compile_error.t) result

  val inspect: t -> string
end

val parse_incremental: Token_stream.t -> Result.t
