module CompileError: sig
  type t

  val to_string: t -> string
  val message: t -> string
end

val analyze: Module.t -> Form.t list -> (Node.t, CompileError.t) result list
