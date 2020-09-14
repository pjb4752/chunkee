open Printf
open Thwack.Extensions.Option
open Thwack.Extensions.Option.Syntax

module VariableNames = Set.Make(Identifier)
module VariableTypes = Map.Make(Identifier)

module TypeNames = Set.Make(Identifier)
module TypeConstructors = Map.Make(Identifier)

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
let find_var { variable_names; variable_types; _ } name =
  let* name = VariableNames.find_opt name variable_names in
  match VariableTypes.find_opt name variable_types with
  | Some tipe -> return (Var.make name tipe)
  | None -> assert false

let var_exists modul var_name =
  is_some @@ find_var modul var_name

let find_type modul type_name =
  match find_var modul type_name with
  | None -> None
  | Some var -> begin
    let tipe = Var.tipe var in
    match tipe with
    | Record _ -> Some tipe
    | _ -> None
  end

let type_exists modul type_name =
  is_some @@ find_type modul type_name

let define_var modul name tipe =
  let variable_names = VariableNames.add name modul.variable_names in
  let variable_types = VariableTypes.add name tipe modul.variable_types in
  { modul with variable_names = variable_names; variable_types = variable_types }

let inspect { name; variable_names; variable_types; } =
  let inspect_var name =
    let variable_type = VariableTypes.find_opt name variable_types in
    match variable_type with
    | Some tipe -> sprintf "Some(Var(%s, %s))" (Identifier.inspect name) (Type.inspect tipe)
    | None -> "None"
  in
  let variable_names = VariableNames.elements variable_names in
  let variable_types = String.concat " " (List.map inspect_var variable_names) in
  let name = Module_name.to_string name in
  sprintf "Module{ name: %s; variable_types: %s }" name variable_types
