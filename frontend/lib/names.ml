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
    | LocalName of string
    | ModuleName of Module_name.t * string

  let inspect = function
    | LocalName name -> sprintf "LocalName(%s)" name
    | ModuleName (module_name, name) -> begin
      sprintf "ModuleName(%s, %s)" (Module_name.inspect module_name) name
    end
end
