module Name = Id

module Var: sig
  module Name = Id
  type t
end

type t

val make: Name.t -> t

val find_var: t -> Var.Name.t -> Var.t option

val add_var: t -> Var.t -> t
