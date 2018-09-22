open Printf
open Thwack.Option

module VarNames = Set.Make(Var.Name)
module VarTypes = Map.Make(Var.Name)

type t = {
  name: Mod_name.t;
  var_names: VarNames.t;
  var_types: Type.t VarTypes.t;
  types: Type.t list;
}

let from_name name = {
  name;
  var_names = VarNames.empty;
  var_types = VarTypes.empty;
  types = [] }

let from_parts path name = from_name (Mod_name.make path name)

let name { name; } = name

let short_name { name; } = Mod_name.short_name name

let path_list { name; } = Mod_name.path_list name

(*TODO use Map.find_else ?*)
let find_var { var_names; var_types } name =
  (VarNames.find_opt name var_names) >>= fun name ->
  match VarTypes.find_opt name var_types with
  | None -> return (Var.declare name)
  | Some t -> return (Var.define name t)

let var_exists modul var_name =
  is_some @@ find_var modul var_name

let find_type { name; types; } type_name =
  List.find_opt (fun t ->
    match t with
    | Type.Rec (mn, n) when mn = name && type_name = n -> true
    | _ -> false
  ) types

let type_exists modul type_name =
  Thwack.Option.is_some @@ find_type modul type_name

let declare_var modul name =
  let var_names = VarNames.add name modul.var_names in
  { modul with var_names = var_names }

let define_var modul name tipe =
  let var_names = VarNames.add name modul.var_names in
  let var_types = VarTypes.add name tipe modul.var_types in
  { modul with var_names = var_names; var_types = var_types }

let define_type modul tipe =
  { modul with types = tipe :: modul.types }

let to_string { name; var_names; var_types; types } =
  let var_to_string vn =
    match VarTypes.find_opt vn var_types with
    | None -> sprintf "%s %s" (Var.Name.to_string vn) "none"
    | Some t -> sprintf "%s %s" (Var.Name.to_string vn) (Type.to_string t) in
  let var_names = VarNames.elements var_names in
  let var_names = String.concat " " (List.map var_to_string var_names) in
  let types = String.concat " " (List.map Type.to_string types) in
  let name = Mod_name.to_string name in
  sprintf "(module %s (vars [%s]) (types [%s])" name var_names types
