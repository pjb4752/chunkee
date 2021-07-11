module Unresolved_name: sig
  type t =
    | UnqualifiedName of string
    | QualifiedName of Module_name.t * string

  val inspect: t -> string
end

module Resolved_name: sig
  type t =
    | LocalName of string
    | ModuleName of Module_name.t * string

  val inspect: t -> string
end
