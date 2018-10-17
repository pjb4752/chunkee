open Printf
open Thwack.Option

type t = {
  pervasive: Pervasive.t;
  tree: Module_tree.t;
  modul: Module.t;
}

type exists_in_scope = string -> bool

type exists_in_decls = Type.Name.t -> Type.t option

let make pervasive modul =
  let tree = Module_tree.with_pervasive pervasive in
  { pervasive; tree; modul }

let current_module { modul } = modul

let undefined_name_error name =
  Error (Cmpl_err.NameError (sprintf "%s is undefined" name))

let undefined_module_error mod_name =
  let mod_name = Mod_name.to_string mod_name in
  Error (Cmpl_err.NameError (sprintf "unknown module %s" mod_name))

let resolve_module_name modul name =
  let name = Var.Name.from_string name in
  let mod_name = Module.name modul in
  if Module.var_exists modul name then Ok (Name.Var.Module (mod_name, name))
  else undefined_name_error (Var.Name.to_string name)

let resolve_qualified_name { tree; modul } mod_name name =
  match Module_tree.find_module tree mod_name with
  | Some modul -> resolve_module_name modul name
  | None -> undefined_module_error mod_name

let resolve_unqualified_name { pervasive; modul } name =
  match resolve_module_name pervasive.modul name with
  | Error _ -> resolve_module_name modul name
  | Ok name -> Ok name

let resolve_name table exists_in_scope = function
  | Name_expr.QualName (mod_name, name) ->
      resolve_qualified_name table mod_name name
  | Name_expr.BareName name -> begin
    if exists_in_scope name then Ok (Name.Var.Local name)
    else resolve_unqualified_name table name
  end

let resolve_module_type modul tipe =
  let name = Type.Name.from_string tipe in
  match Module.find_type modul name with
  | Some tipe -> Ok tipe
  | None -> undefined_name_error tipe

let resolve_qualified_type tree modul mod_name tipe =
  match Module_tree.find_module tree mod_name with
  | Some modul -> resolve_module_type modul tipe
  | None -> undefined_module_error mod_name

let resolve_unqualified_type modul lookup_fn tipe =
  match Type.find_builtin tipe with
  | Some tipe -> Ok tipe
  | None -> begin
    let name = Type.Name.from_string tipe in
    let maybe_type =
      lookup_fn >>= fun lookup_fn ->
      (lookup_fn name) >>= fun tipe ->
      return tipe in
    match maybe_type with
    | Some tipe -> Ok tipe
    | None -> resolve_module_type modul tipe
  end

let resolve_simple_type { tree; modul } lookup_fn = function
  | Name_expr.QualName (mod_name, tipe) ->
      resolve_qualified_type tree modul mod_name tipe
  | Name_expr.BareName tipe ->
      resolve_unqualified_type modul lookup_fn tipe

let resolve_type table ?lookup_fn:(lookup_fn=None) = function
  | Type_expr.SimpleType tipe -> resolve_simple_type table lookup_fn tipe
  | Type_expr.FnType ts -> Ok (Type.Num)

let select_module { tree; modul } mod_name =
  if (Module.name modul) = mod_name then Some modul
  else Module_tree.find_module tree mod_name

let module_type table mod_name var_name =
  (select_module table mod_name) >>= fun modul ->
  (Module.find_var modul var_name) >>= fun var ->
  (Var.tipe var) >>= fun tipe ->
  return tipe

let define_record table name fields =
  let new_modul = Module.define_record table.modul name fields in
  { table with modul = new_modul }

let to_string { tree; modul } =
  let tree = Module_tree.to_string tree in
  let modul = Module.to_string modul in
  sprintf "(symbol-table (tree %s) (current %s))" tree modul
