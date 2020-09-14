open Printf

module V = Var

module Var = struct
  type t =
    | Local of string
    | Module of Module_name.t * Identifier.t

  let to_string = function
    | Local s -> sprintf "(local %s)" s
    | Module (qualified_name, var_name) -> sprintf "(module %s/%s)"
      (Module_name.to_string qualified_name) (Identifier.to_string var_name)

  let inspect = function
    | Local var -> sprintf "Local(%s)" var
    | Module (qualified_name, var_name) -> begin
      sprintf "Module (%s, %s)" (Module_name.inspect qualified_name) (Identifier.inspect var_name)
    end
end
