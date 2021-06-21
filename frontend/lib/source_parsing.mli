module Result: sig
  type value_t = (Source_form.t, Compile_error.t) result
  type t = value_t list

  val inspect: t -> string
end

val parse_incremental: Token_stream.t -> Result.t
