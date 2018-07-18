module Define : sig
  type t = (Module.t, Cmpl_err.t) result

  val define_var: Module.t -> Parse.t -> t

  val define_vars: Module.t -> Parse.t list -> t
end

module Resolve : sig
  type node_t = Name.t Node.t

  type s = (node_t, Cmpl_err.t) result
  type t = (node_t list, Cmpl_err.t) result

  val resolve_node: Symbol_table.t -> Module.t -> Parse.t -> s

  val resolve_names: Symbol_table.t -> Module.t -> Parse.t list -> t

end
