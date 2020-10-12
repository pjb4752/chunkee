open Printf

type t =
  | BareName of string
  | QualName of Module_name.t * string

let inspect = function
  | BareName name -> sprintf "BareName(%s)" name
  | QualName (module_name, name) -> begin
    let module_name = Module_name.inspect module_name in
    sprintf "QualName(%s, %s)" module_name name
  end
