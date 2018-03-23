module NameError: sig
  type t

  val message: t -> string
  val to_string: t -> string
end

val define_vars: Module.t -> Node.t -> (Module.t, NameError.t) result
