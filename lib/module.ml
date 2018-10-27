open Printf
open Thwack.Option

module VarNames = Set.Make(Var.Name)
module VarTypes = Map.Make(Var.Name)

module TypeNames = Set.Make(Type.Name)
module TypeCons = Map.Make(Type.Name)

type t = {
  name: Mod_name.t;
  var_names: VarNames.t;
  var_types: Type.t VarTypes.t;
  type_names: TypeNames.t;
  type_cons: Type.rec_cons_t TypeCons.t;
}

let from_name name = {
  name;
  var_names = VarNames.empty;
  var_types = VarTypes.empty;
  type_names = TypeNames.empty;
  type_cons = TypeCons.empty;
}

let from_parts path name = from_name (Mod_name.make path name)

let name { name; } = name

let short_name { name; } = Mod_name.short_name name

let path_list { name; } = Mod_name.path_list name

(*TODO use Map.find_else ?*)
let find_var { var_names; var_types } name =
  (VarNames.find_opt name var_names) >>= fun name ->
  match VarTypes.find_opt name var_types with
  | Some tipe -> return (Var.make name tipe)
  | None -> assert false

let var_exists modul var_name =
  is_some @@ find_var modul var_name

let find_type { name; type_names; type_cons } type_name =
  match TypeNames.find_opt type_name type_names with
  | None -> None
  | Some type_name -> begin
    match TypeCons.find_opt type_name type_cons with
    | None -> assert false
    | Some cons -> Some (Type.Rec (name, type_name, cons))
  end

let type_exists modul type_name =
  Thwack.Option.is_some @@ find_type modul type_name

let define_var modul name tipe =
  let var_names = VarNames.add name modul.var_names in
  let var_types = VarTypes.add name tipe modul.var_types in
  { modul with var_names = var_names; var_types = var_types }

let define_record modul type_name cons =
  let new_names = TypeNames.add type_name modul.type_names in
  let new_cons = TypeCons.add type_name cons modul.type_cons in
  { modul with type_names = new_names; type_cons = new_cons }

let to_string { name; var_names; var_types; type_names } =
  let var_to_string vn =
    match VarTypes.find_opt vn var_types with
    | None -> sprintf "%s %s" (Var.Name.to_string vn) "none"
    | Some t -> sprintf "%s %s" (Var.Name.to_string vn) (Type.to_string t) in
  let type_to_string tn =
    Type.Name.to_string tn in
  let var_names = VarNames.elements var_names in
  let var_names = String.concat " " (List.map var_to_string var_names) in
  let type_names = TypeNames.elements type_names in
  let type_names = String.concat " " (List.map type_to_string type_names) in
  let name = Mod_name.to_string name in
  sprintf "(module %s (vars [%s]) (types [%s])" name var_names type_names
