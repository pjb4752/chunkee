open Printf
open Thwack.Extensions
open Thwack.Result

module Scope = Set.Make(String)

module PNode = Ast.Parsed_node
module RNode = Ast.Resolved_node

type r_node_rt = (RNode.t, Cmpl_err.t) result

type ts = (Module.t * RNode.t list, Cmpl_err.t) result

let undefined_name_error name =
  Error (Cmpl_err.NameError (sprintf "%s is undefined" name))

let undefined_module_error mod_name =
  let mod_name = Mod_name.to_string mod_name in
  Error (Cmpl_err.NameError (sprintf "unknown module %s" mod_name))

let make_symlit modul m_name =
  let mod_name = Module.name modul in
  let name = Name.Var.Module (mod_name, m_name) in
  Ok (RNode.SymLit name)

let resolve_pervasive_name table name =
  let name = Var.Name.from_string name in
  let pervasive = Symbol_table.pervasive_module table in
  if Module.var_exists pervasive name then Some (pervasive, name)
  else None

let resolve_module_name modul name =
  let name = Var.Name.from_string name in
  if Module.var_exists modul name then make_symlit modul name
  else undefined_name_error (Var.Name.to_string name)

let resolve_qualified_name table modul mod_name name =
  match Symbol_table.find_module table mod_name with
  | Some modul -> resolve_module_name modul name
  | None -> undefined_module_error mod_name

let resolve_unqualified_name table modul name =
  match resolve_pervasive_name table name with
  | Some (mod_name, name) -> make_symlit mod_name name
  | None -> resolve_module_name modul name

let resolve_name table modul scopes = function
  | Name_expr.QualName (mod_name, name) ->
      resolve_qualified_name table modul mod_name name
  | Name_expr.BareName name ->
    if List.exists (Scope.mem name) scopes then
      Ok (RNode.SymLit (Name.Var.Local name))
    else resolve_unqualified_name table modul name

let resolve_module_type modul tipe =
  let name = Type.Name.from_string tipe in
  match Module.find_type modul name with
  | Some tipe -> Ok tipe
  | None -> undefined_name_error tipe

let resolve_qualified_type table modul mod_name tipe =
  match Symbol_table.find_module table mod_name with
  | Some modul -> resolve_module_type modul tipe
  | None -> undefined_module_error mod_name

let resolve_unqualified_type modul tipe =
  match Type.find_builtin tipe with
  | Some tipe -> Ok tipe
  | None -> resolve_module_type modul tipe

let resolve_simple_type table modul = function
  | Name_expr.QualName (mod_name, tipe) ->
      resolve_qualified_type table modul mod_name tipe
  | Name_expr.BareName tipe ->
    resolve_unqualified_type modul tipe

let resolve_type table modul = function
  | Type_expr.SimpleType tipe -> resolve_simple_type table modul tipe
  | Type_expr.FnType ts -> Ok (Type.Num)

let resolve_rec table modul name fields =
  let fold_fn field fields =
    fields >>= fun fields ->
    let (name, tipe) = PNode.VarDef.to_tuple field in
    (resolve_type table modul tipe) >>= fun tipe ->
    let field = RNode.VarDef.from_parts name tipe in
    return (field :: fields) in
  let fields = List.fold_right fold_fn fields (Ok []) in
  fields >>= fun fields ->
  let field_tuples = List.map RNode.VarDef.to_tuple fields in
  let modul = Module.define_record modul name field_tuples in
  return (modul, RNode.Rec (name, fields))

let resolve_def recur_fn scopes name expr =
  (recur_fn scopes expr) >>= fun expr ->
  return (RNode.Def (name, expr))

let resolve_fn recur_fn table modul scopes params body =
  let fold_fn var vars =
    vars >>= fun vars ->
    let (name, tipe) = PNode.VarDef.to_tuple var in
    (resolve_type table modul tipe) >>= fun t ->
    let var = RNode.VarDef.from_parts name t in
    return (var :: vars) in
  let map_fn var =
    let (name, _) = RNode.VarDef.to_tuple var in
    RNode.VarDef.Name.to_string name in
  let params = List.fold_right fold_fn params (Ok []) in
  params >>= fun params ->
    let param_names = List.map map_fn params in
    let fn_scope = Scope.of_list param_names in
    let scopes = fn_scope :: scopes in
    (recur_fn scopes body) >>= fun body ->
    return (RNode.Fn (params, body))

let resolve_if recur_fn scopes tst iff els =
  (recur_fn scopes tst) >>= fun tst ->
  (recur_fn scopes iff) >>= fun iff ->
  (recur_fn scopes els) >>= fun els ->
  return (RNode.If (tst, iff, els))

let resolve_binding recur_fn scopes binding =
  let (name, expr) = PNode.Binding.to_tuple binding in
  match recur_fn scopes expr with
  | Error e -> Error e
  | Ok expr -> begin
    (*TODO we should always create new scope for each binding*)
    let binding = RNode.Binding.from_node name expr in
    let current_scope = List.hd_else scopes Scope.empty in
    let name = PNode.Binding.Name.to_string name in
    let new_scope = Scope.add name current_scope in
    match scopes with
    | [] -> Ok ([new_scope], binding)
    | hd :: tl -> Ok (new_scope :: tl, binding)
  end

let resolve_let recur_fn scopes bindings body =
  let rec resolve_bindings' scopes resolved = function
    | [] -> Ok (scopes, resolved)
    | b :: bs -> begin
      match resolve_binding recur_fn scopes b with
      | Error e -> Error e
      | Ok (s, b) -> resolve_bindings' s (b :: resolved) bs
    end in
  match resolve_bindings' scopes [] bindings with
  | Error e -> Error e
  | Ok (scopes, bindings) -> begin
    (recur_fn scopes body) >>= fun body ->
    return (RNode.Let (List.rev bindings, body))
  end

let resolve_apply recur_fn scopes fn args =
  let fold_fn resolved arg =
    resolved >>= fun r ->
    (recur_fn scopes arg) >>= fun a ->
    return (a :: r) in
  let args = List.fold_left fold_fn (Ok []) args in
  args >>= fun args ->
  (recur_fn scopes fn) >>= fun fn ->
  return (RNode.Apply (fn, List.rev args))

let resolve_cast recur_fn table modul scopes tipe expr =
  (resolve_type table modul tipe) >>= fun tipe ->
  (recur_fn scopes expr) >>= fun expr ->
  return (RNode.Cast (tipe, expr))

let resolve_node table modul node =
  let rec resolve' scopes = function
    | PNode.Rec _ -> assert false
    | PNode.NumLit n -> Ok (RNode.NumLit n)
    | PNode.StrLit s -> Ok (RNode.StrLit s)
    | PNode.SymLit name -> resolve_name table modul scopes name
    | PNode.Def (var, expr) -> resolve_def resolve' scopes var expr
    | PNode.Fn (params, body) -> resolve_fn resolve' table modul scopes params body
    | PNode.If (tst, iff, els) -> resolve_if resolve' scopes tst iff els
    | PNode.Let (bindings, body) -> resolve_let resolve' scopes bindings body
    | PNode.Apply (fn, args) -> resolve_apply resolve' scopes fn args
    | PNode.Cast (tipe, expr) -> resolve_cast resolve' table modul scopes tipe expr in
  resolve' [] node

let resolve_nodes table modul nodes =
  let fold_fn node nodes =
    nodes >>= fun nodes ->
    (resolve_node table modul node) >>= fun node ->
    return (node :: nodes) in
  List.fold_right fold_fn nodes (Ok [])

let resolve_typedef table modul = function
  | PNode.Rec (name, fields) -> resolve_rec table modul name fields
  | _ -> assert false

let resolve_typedefs table modul nodes =
  let fold_fn node accumulator =
    accumulator >>= fun (modul, nodes) ->
    (resolve_typedef table modul node) >>= fun (modul, node) ->
    return (modul, node :: nodes) in
  List.fold_right fold_fn nodes (Ok (modul, []))

let partition_nodes nodes =
  List.partition (fun e ->
    match e with
    | PNode.Rec _ -> true
    | _ -> false) nodes

let resolve table modul nodes =
  let (typedefs, rest) = partition_nodes nodes in
  (resolve_typedefs table modul typedefs) >>= fun (modul, typedefs) ->
  (resolve_nodes table modul rest) >>= fun rest ->
  return (modul, List.append typedefs rest)
