module Result : sig
  type t = (Symbol_table.t, Compile_error.t) result

  val inspect: t -> string
end

val define_identifiers: Symbol_table.t -> Ast.Parsed_node.t -> Result.t
