open Printf
open Thwack.Extensions.Option
open Thwack.Extensions.Option.Syntax

module VarNames = Set.Make(Identifier)
module VarTypes = Map.Make(Identifier)

module TypeNames = Set.Make(Identifier)
module TypeCons = Map.Make(Identifier)

type t = {
  name: Module_name.t;
  var_names: VarNames.t;
  var_types: Type.t VarTypes.t;
  type_names: TypeNames.t;
  type_cons: Type.rec_cons_t TypeCons.t;
}

let with_name name = {
  name;
  var_names = VarNames.empty;
  var_types = VarTypes.empty;
  type_names = TypeNames.empty;
  type_cons = TypeCons.empty;
}

let with_path_and_base path base = with_name (Module_name.from_path_and_base path base)

let name { name; _ } = name

let basename { name; _ } = Module_name.base name

let path_segments { name; _ } = Module_name.path_segments name

(*TODO use Map.find_else ?*)
let find_var { var_names; var_types; _ } name =
  let* name = VarNames.find_opt name var_names in
  match VarTypes.find_opt name var_types with
  | Some tipe -> return (Var.make name tipe)
  | None -> assert false

let var_exists modul var_name =
  is_some @@ find_var modul var_name

let find_type { name; type_names; type_cons; _ } type_name =
  match TypeNames.find_opt type_name type_names with
  | None -> None
  | Some type_name -> begin
    match TypeCons.find_opt type_name type_cons with
    | None -> assert false
    | Some cons -> Some (Type.Rec (name, type_name, cons))
  end

let type_exists modul type_name =
  is_some @@ find_type modul type_name

let define_var modul name tipe =
  let var_names = VarNames.add name modul.var_names in
  let var_types = VarTypes.add name tipe modul.var_types in
  { modul with var_names = var_names; var_types = var_types }

let define_record modul type_name cons =
  let new_names = TypeNames.add type_name modul.type_names in
  let new_cons = TypeCons.add type_name cons modul.type_cons in
  { modul with type_names = new_names; type_cons = new_cons }

let to_string { name; var_names; var_types; type_names; _ } =
  let var_to_string vn =
    match VarTypes.find_opt vn var_types with
    | None -> sprintf "%s %s" (Identifier.to_string vn) "none"
    | Some t -> sprintf "%s %s" (Identifier.to_string vn) (Type.to_string t) in
  let type_to_string tn =
    Identifier.to_string tn in
  let var_names = VarNames.elements var_names in
  let var_names = String.concat " " (List.map var_to_string var_names) in
  let type_names = TypeNames.elements type_names in
  let type_names = String.concat " " (List.map type_to_string type_names) in
  let name = Module_name.to_string name in
  sprintf "(module %s (vars [%s]) (types [%s])" name var_names type_names
