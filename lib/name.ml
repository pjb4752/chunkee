open Printf

module V = Var

module Var = struct
  type t =
    | Local of string
    | Module of Mod_name.t * V.Name.t

  let to_string = function
    | Local s -> sprintf "(local %s)" s
    | Module (qn, vn) -> sprintf "(module %s/%s)"
      (Mod_name.to_string qn) (V.Name.to_string vn)

  let inspect = function
    | Local var -> sprintf "Local(%s)" var
    | Module (qualified_name, var_name) -> begin
      sprintf "Module (%s, %s)" (Mod_name.inspect qualified_name) (V.Name.to_string var_name)
    end
end
