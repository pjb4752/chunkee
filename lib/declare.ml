open Printf
open Thwack.Result

module Node = Ast.Parsed_node

type t = (Symbol_table.t, Cmpl_err.t) result

module TypeDecls = Map.Make(Type.Name)

let var_exists table name =
  let mod_name = Symbol_table.current_module table |> Module.name in
  match Symbol_table.module_var table mod_name name with
  | Some _ -> true
  | None -> false

let find_fn_type table params rtype =
  let ptypes = List.map (fun p ->
    let (_, tipe) = Node.VarDef.to_tuple p in tipe) params in
  let ftype = Type_expr.FnType (List.append ptypes [rtype]) in
  Symbol_table.resolve_type table ftype

let find_def_type table = function
  | Node.NumLit _ -> Ok Type.Num
  | Node.StrLit _ -> Ok Type.Str
  | Node.Fn (params, rtype, _) -> find_fn_type table params rtype
  | _ -> assert false

let define_var table = function
  | Node.Def (name, expr) -> begin
    if var_exists table name then
      let name = Node.Name.to_string name in
      let message = sprintf "var %s already declared" name in
      Error (Cmpl_err.NameError message)
    else (
      (find_def_type table expr) >>= fun tipe ->
      return (Symbol_table.define_var table name tipe)
    )
  end
  | _ -> assert false

let define_vars table vardefs =
  List.fold_right (fun vardef table ->
    table >>= fun table ->
    (define_var table vardef) >>= fun table ->
    return table) vardefs (Ok table)

let type_redef_error name =
  let name = Type.Name.to_string name in
  let message = sprintf "type %s already declared" name in
  Error (Cmpl_err.NameError message)

let declare_type mod_name typedecls = function
  | Node.Rec (name, _) ->
      if TypeDecls.mem name typedecls then type_redef_error name
      else Ok (TypeDecls.add name (Type.Rec (mod_name, name)) typedecls)
  | _ -> assert false

let declare_types mod_name typedefs =
  List.fold_right (fun typedef typedecls ->
    typedecls >>= fun typedecls ->
    (declare_type mod_name typedecls typedef) >>= fun typedecls ->
    return typedecls) typedefs (Ok TypeDecls.empty)

let resolve_type table typedecls type_expr =
  Symbol_table.resolve_type table ~lookup_fn:(Some (fun name ->
    TypeDecls.find_opt name typedecls)) type_expr

let resolve_record_fields table typedecls fields =
  let fold_fn field fields =
    fields >>= fun fields ->
    let (name, type_expr) = Node.VarDef.to_tuple field in
    let name = Node.VarDef.Name.to_string name in
    (resolve_type table typedecls type_expr) >>= fun tipe ->
    (return ((Type.Name.from_string name, tipe) :: fields)) in
  List.fold_right fold_fn fields (Ok [])

let define_record table typedecls name fields =
  (resolve_record_fields table typedecls fields) >>= fun resolved ->
  return (Symbol_table.define_record table name resolved)

let define_type table typedecls = function
  | Node.Rec (name, fields) -> define_record table typedecls name fields
  | _ -> assert false

let define_types table typedefs =
  let mod_name = Symbol_table.current_module table |> Module.name in
  match declare_types mod_name typedefs with
  | Error e -> Error e
  | Ok typedecls -> begin
    let fold_fn typedef table =
      table >>= fun table ->
      (define_type table typedecls typedef) >>= fun table ->
      return table in
    List.fold_right fold_fn typedefs (Ok table)
  end

let partition_defs nodes =
  List.partition (fun n ->
    match n with
    | Node.Rec _ -> true
    | _ -> false) nodes

let declare_toplevels table nodes =
  let (typedefs, vardefs) = partition_defs nodes in
  (define_types table typedefs) >>= fun table ->
  (define_vars table vardefs) >>= fun table ->
  return table
