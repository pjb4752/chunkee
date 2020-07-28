open Printf

type t =
  | BareName of string
  | QualName of Mod_name.t * string

let bare_name_to_string name =
  sprintf "(bare-name %s)" name

let qual_name_to_string mod_name name =
  let mod_name = Mod_name.to_string mod_name in
  sprintf "(qual-name %s/%s)" mod_name name

let to_string = function
  | BareName name -> bare_name_to_string name
  | QualName (mod_name, name) -> qual_name_to_string mod_name name

let inspect = function
  | BareName name -> sprintf "BareName(%s)" name
  | QualName (module_name, name) -> begin
    let module_name = Mod_name.inspect module_name in
    sprintf "QualName(%s, %s)" module_name name
  end
