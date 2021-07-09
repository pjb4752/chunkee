module Result : sig
  type t = (Ast.Resolved_form.t, Compile_error.t) result

  val inspect: t -> string
end

val resolve_identifiers: Symbol_table.t -> Ast.Semantic_form.t -> Result.t
