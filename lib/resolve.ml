open Printf
open Thwack.Extensions
open Thwack.Result

module Scope = Set.Make(String)

module PNode = Ast.Parsed_node
module RNode = Ast.Resolved_node

type t = (RNode.t list, Cmpl_err.t) result

let resolve_symlit table scopes name =
  let name = Symbol_table.resolve_name table (fun name ->
    List.exists (Scope.mem name) scopes) name in
  name >>= fun name -> return (RNode.SymLit name)

let resolve_rec table name fields =
  let fold_fn field fields =
    fields >>= fun fields ->
    let (name, tipe) = PNode.VarDef.to_tuple field in
    (Symbol_table.resolve_type table tipe) >>= fun tipe ->
    let field = RNode.VarDef.from_parts name tipe in
    return (field :: fields) in
  (List.fold_right fold_fn fields (Ok [])) >>= fun fields ->
  return (RNode.Rec (name, fields))

let resolve_def recur_fn scopes name expr =
  (recur_fn scopes expr) >>= fun expr ->
  return (RNode.Def (name, expr))

let resolve_fn recur_fn table scopes params rtype body =
  let fold_fn var vars =
    vars >>= fun vars ->
    let (name, tipe) = PNode.VarDef.to_tuple var in
    (Symbol_table.resolve_type table tipe) >>= fun t ->
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
    (Symbol_table.resolve_type table rtype) >>= fun rtype ->
    (recur_fn scopes body) >>= fun body ->
    return (RNode.Fn (params, rtype, body))

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

let resolve_cast recur_fn table scopes tipe expr =
  (Symbol_table.resolve_type table tipe) >>= fun tipe ->
  (recur_fn scopes expr) >>= fun expr ->
  return (RNode.Cast (tipe, expr))

let resolve_node table node =
  let rec resolve' scopes = function
    | PNode.NumLit n -> Ok (RNode.NumLit n)
    | PNode.StrLit s -> Ok (RNode.StrLit s)
    | PNode.SymLit name -> resolve_symlit table scopes name
    | PNode.Rec (name, fields) -> resolve_rec table name fields
    | PNode.Def (var, expr) -> resolve_def resolve' scopes var expr
    | PNode.Fn (params, rtype, body) ->
        resolve_fn resolve' table scopes params rtype body
    | PNode.If (tst, iff, els) -> resolve_if resolve' scopes tst iff els
    | PNode.Let (bindings, body) -> resolve_let resolve' scopes bindings body
    | PNode.Apply (fn, args) -> resolve_apply resolve' scopes fn args
    | PNode.Cast (tipe, expr) -> resolve_cast resolve' table scopes tipe expr in
  resolve' [] node

let resolve_nodes table nodes =
  let fold_fn node nodes =
    nodes >>= fun nodes ->
    (resolve_node table node) >>= fun node ->
    return (node :: nodes) in
  List.fold_right fold_fn nodes (Ok [])

let resolve table nodes =
  resolve_nodes table nodes
