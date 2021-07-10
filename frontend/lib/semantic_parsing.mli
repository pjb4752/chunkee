module Result : sig
  type t = (Ast.Semantic_form.t, Compile_error.t) result

  val inspect: t -> string
end

val parse_form: Source_form.t -> Result.t
