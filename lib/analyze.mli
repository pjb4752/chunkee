module CompileError: sig
  type t

  val message: t -> string
  val to_string: t -> string
end

val analyze: Form.t -> (Node.t, CompileError.t) result
