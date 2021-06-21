module Node = Ast.Parsed_node

module Result : sig
  type t = (Node.t, Compile_error.t) result

  val inspect: t -> string
end

val parse_form: ?check_toplevel: bool -> Source_form.t -> Result.t
