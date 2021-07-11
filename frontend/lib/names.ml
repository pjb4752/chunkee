open Printf

module Unresolved_name = struct
  type t =
    | UnqualifiedName of string
    | QualifiedName of Module_name.t * string

  let inspect = function
    | UnqualifiedName name -> sprintf "UnqualifiedName(%s)" name
    | QualifiedName (module_name, name) -> begin
      let module_name = Module_name.inspect module_name in
      sprintf "QualifiedName(%s, %s)" module_name name
    end
end

module Resolved_name = struct
  type t =
    | LocalName of Identifier.t
    | ModuleName of Module_name.t * Identifier.t

  let inspect = function
    | LocalName identifier -> sprintf "LocalName(%s)" (Identifier.inspect identifier)
    | ModuleName (qualified_name, identifier) -> begin
      sprintf "ModuleName(%s, %s)" (Module_name.inspect qualified_name) (Identifier.inspect identifier)
    end
end
