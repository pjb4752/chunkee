open Printf

type t =
  | BareName of string
  | QualName of Module_name.t * string

let bare_name_to_string name =
  sprintf "(bare-name %s)" name

let qual_name_to_string module_name name =
  let module_name = Module_name.to_string module_name in
  sprintf "(qual-name %s/%s)" module_name name

let to_string = function
  | BareName name -> bare_name_to_string name
  | QualName (module_name, name) -> qual_name_to_string module_name name

let inspect = function
  | BareName name -> sprintf "BareName(%s)" name
  | QualName (module_name, name) -> begin
    let module_name = Module_name.inspect module_name in
    sprintf "QualName(%s, %s)" module_name name
  end
