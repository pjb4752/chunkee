open Printf
open Common.Extensions.Option
open Common.Extensions.Option.Syntax

module VariableNames = Set.Make(String)
module VariableTypes = Map.Make(String)

module TypeNames = Set.Make(String)
module TypeConstructors = Map.Make(String)

type t = {
  name: Module_name.t;
  variable_names: VariableNames.t;
  variable_types: Type.t VariableTypes.t;
}

let with_name name = {
  name;
  variable_names = VariableNames.empty;
  variable_types = VariableTypes.empty;
}

let with_path_and_base path base = with_name (Module_name.from_path_and_base path base)

let name { name; _ } = name

let basename { name; _ } = Module_name.base name

let path_segments { name; _ } = Module_name.path_segments name

(*TODO use Map.find_else ?*)
let find_variable { variable_names; variable_types; _ } requested_name =
  let* found_name = VariableNames.find_opt requested_name variable_names in
  match VariableTypes.find_opt found_name variable_types with
  | Some found_type -> return (Var.make found_name found_type)
  | None -> assert false

let variable_exists module_to_search variable_name =
  is_some @@ find_variable module_to_search variable_name

let find_type module_to_search type_name =
  match find_variable module_to_search type_name with
  | None -> None
  | Some variable -> begin
    let variable_type = Var.tipe variable in
    match variable_type with
    | Record _ -> Some variable_type
    | _ -> None
  end

let type_exists module_to_search type_name =
  is_some @@ find_type module_to_search type_name

let define_variable target_module variable_name variable_type =
  let variable_names = VariableNames.add variable_name target_module.variable_names in
  let variable_types = VariableTypes.add variable_name variable_type target_module.variable_types in
  { target_module with variable_names; variable_types }

let inspect { name; variable_names; variable_types; } =
  let inspect_variable name =
    let variable_type = VariableTypes.find_opt name variable_types in
    match variable_type with
    | Some found_type -> sprintf "Some(Var(%s, %s))" name (Type.inspect found_type)
    | None -> "None"
  in
  let variable_names = VariableNames.elements variable_names in
  let variable_types = String.concat " " (List.map inspect_variable variable_names) in
  let name = Module_name.to_string name in
  sprintf "Module{ name: %s; variable_types: %s }" name variable_types
