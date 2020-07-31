open Printf
open Thwack.Extensions.Result
open Thwack.Extensions.Result.Syntax

module Node = Ast.Parsed_node

type t = (Symbol_table.t, Cmpl_err.t) result

type declarations =
  | Record of Module_name.t

module TypeDecls = Map.Make(Identifier)

let build_prefix { Metadata.line_num; char_num } =
  sprintf "in expression at %d:%d" line_num char_num

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
  | Error e ->
      let prefix = build_prefix metadata in
      Error (Cmpl_err.definition_errors metadata prefix [
        Symbol_table.err_string e
      ])

let find_def_type table expr metadata =
  match expr with
  | Node.NumLit _ -> Ok Type.Num
  | Node.StrLit _ -> Ok Type.Str
  | Node.Fn (params, rtype, _, _) -> find_fn_type table params rtype metadata
  | Node.Cons (rtype, _, _) -> begin
    match Symbol_table.resolve_type table rtype with
    | Ok tipe -> Ok tipe
    | Error e ->
        let prefix = build_prefix metadata in
        Error (Cmpl_err.definition_errors metadata prefix [
          Symbol_table.err_string e
        ])
  end
  | _ -> assert false

let define_var table = function
  | Node.Def (name, expr, metadata) -> begin
    if var_exists table name then
      let prefix = build_prefix metadata in
      Error (Cmpl_err.definition_errors metadata prefix [
        sprintf "var with name '%s' is already defined" @@ Identifier.to_string name
      ])
    else (
      let* tipe = find_def_type table expr metadata in
      return (Symbol_table.define_var table name tipe)
    )
  end
  | _ -> assert false

let define_vars table vardefs =
  List.fold_right (fun vardef table ->
    let* table = table in
    let* table = define_var table vardef in
    return table) vardefs (Ok table)

let declare_type typedecls mod_name = function
  | Node.Rec (name, _, metadata) ->
      if TypeDecls.mem name typedecls then
        let prefix = build_prefix metadata in
        Error (Cmpl_err.definition_errors metadata prefix [
          sprintf "type with name '%s' is already defined" @@ Identifier.to_string name
        ])
      else Ok (TypeDecls.add name (Record mod_name) typedecls)
  | _ -> assert false

let declare_types mod_name typedefs =
  List.fold_right (fun typedef typedecls ->
    let* typedecls = typedecls in
    let* typedecls = declare_type typedecls mod_name typedef in
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
  | Error e ->
      let prefix = build_prefix metadata in
      Error (Cmpl_err.definition_errors metadata prefix [
        Symbol_table.err_string e
      ])

let resolve_constructor table typedecls fields metadata =
  let fold_fn field fields =
    let* fields = fields in
    let (name, type_expr) = Node.VarDef.to_tuple field in
    let name = Identifier.to_string name in
    let* tipe = resolve_type table typedecls type_expr metadata in
    return ((Identifier.from_string name, tipe) :: fields) in
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
      let* table = table in
      let* table = define_type table typedecls typedef in
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
  let* table = define_types table typedefs in
  let* table = define_vars table vardefs in
  return table
