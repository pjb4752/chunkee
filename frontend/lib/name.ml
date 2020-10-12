open Printf

type t =
  | Local of string
  | Module of Module_name.t * Identifier.t

let inspect = function
  | Local var -> sprintf "Local(%s)" var
  | Module (qualified_name, var_name) -> begin
    sprintf "Module (%s, %s)" (Module_name.inspect qualified_name) (Identifier.inspect var_name)
  end
