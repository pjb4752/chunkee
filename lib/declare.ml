open Printf
open Thwack.Result

module Node = Ast.Parsed_node

type t = (Symbol_table.t, Cmpl_err.t) result

type declarations =
  | Record of Mod_name.t

module TypeDecls = Map.Make(Type.Name)

let var_exists table name =
  let mod_name = Symbol_table.current_module table |> Module.name in
  match Symbol_table.module_var table mod_name name with
  | Some _ -> true
  | None -> false

let find_fn_type table params rtype metadata =
  let ptypes = List.map (fun p ->
    let (_, tipe) = Node.VarDef.to_tuple p in tipe) params in
  let ftype = Type_expr.FnType (List.append ptypes [rtype]) in
  match Symbol_table.resolve_type table ftype with
  | Ok tipe -> Ok tipe
  | Error e -> Error (Cmpl_err.definition_error metadata @@ Symbol_table.err_string e)

let find_def_type table expr metadata =
  match expr with
  | Node.NumLit _ -> Ok Type.Num
  | Node.StrLit _ -> Ok Type.Str
  | Node.Fn (params, rtype, _, _) -> find_fn_type table params rtype metadata
  | Node.Cons (rtype, _, _) -> begin
    match Symbol_table.resolve_type table rtype with
    | Ok tipe -> Ok tipe
    | Error e -> Error (Cmpl_err.definition_error metadata @@ Symbol_table.err_string e)
  end
  | _ -> assert false

let define_var table = function
  | Node.Def (name, expr, meta) -> begin
    if var_exists table name then
      let message = sprintf "\tVar with name '%s' already defined" @@ Node.Name.to_string name in
      Error (Cmpl_err.definition_error meta message)
    else (
      (find_def_type table expr meta) >>= fun tipe ->
      return (Symbol_table.define_var table name tipe)
    )
  end
  | _ -> assert false

let define_vars table vardefs =
  List.fold_right (fun vardef table ->
    table >>= fun table ->
    (define_var table vardef) >>= fun table ->
    return table) vardefs (Ok table)

let declare_type typedecls mod_name = function
  | Node.Rec (name, _, meta) ->
      if TypeDecls.mem name typedecls then
        let message = sprintf "\tType with name '%s' already defined" @@ Type.Name.to_string name in
        Error (Cmpl_err.definition_error meta message)
      else Ok (TypeDecls.add name (Record mod_name) typedecls)
  | _ -> assert false

let declare_types mod_name typedefs =
  List.fold_right (fun typedef typedecls ->
    typedecls >>= fun typedecls ->
    (declare_type typedecls mod_name typedef) >>= fun typedecls ->
    return typedecls) typedefs (Ok TypeDecls.empty)

let resolve_type table typedecls type_expr metadata =
  let resolved = Symbol_table.resolve_type table ~lookup_fn:(Some (fun name ->
    match TypeDecls.find_opt name typedecls with
    | Some (Record mod_name) -> Some (Type.Rec (mod_name, name, []))
    | None -> None
    )) type_expr
  in
  match resolved with
  | Ok tipe -> Ok tipe
  | Error e -> Error (Cmpl_err.definition_error metadata @@ Symbol_table.err_string e)

let resolve_constructor table typedecls fields metadata =
  let fold_fn field fields =
    fields >>= fun fields ->
    let (name, type_expr) = Node.VarDef.to_tuple field in
    let name = Node.VarDef.Name.to_string name in
    (resolve_type table typedecls type_expr metadata) >>= fun tipe ->
    (return ((Type.Name.from_string name, tipe) :: fields)) in
  List.fold_right fold_fn fields (Ok [])

let define_record table typedecls name fields metadata =
  (resolve_constructor table typedecls fields metadata) >>= fun cons_fields ->
  return (Symbol_table.define_record table name cons_fields)

let define_type table typedecls = function
  | Node.Rec (name, fields, meta) -> define_record table typedecls name fields meta
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
