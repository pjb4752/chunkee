open Printf
open Thwack.Extensions
open Thwack.Extensions.Result
open Thwack.Extensions.Result.Syntax

module Scope = Set.Make(String)

module PNode = Ast.Parsed_node
module RNode = Ast.Resolved_node

type t = (RNode.t list, Cmpl_err.t) result

let build_prefix { Metadata.line_num; char_num } =
  sprintf "in expression at %d:%d" line_num char_num

let resolve_symlit table scopes metadata name =
  let name = Symbol_table.resolve_name table (fun name ->
    List.exists (Scope.mem name) scopes) name
  in
  match name with
  | Ok name -> Ok (RNode.SymLit (name, metadata))
  | Error e ->
      let prefix = build_prefix metadata in
      Error (Cmpl_err.name_errors metadata prefix [
        Symbol_table.err_string e
      ])

let resolve_type table tipe metadata =
  match Symbol_table.resolve_type table tipe with
  | Ok tipe -> Ok tipe
  | Error e ->
      let prefix = build_prefix metadata in
      Error (Cmpl_err.name_errors metadata prefix [
        Symbol_table.err_string e
      ])

let resolve_rec table meta name fields =
  let fold_fn field fields =
    let* fields = fields in
    let (name, tipe) = PNode.VarDef.to_tuple field in
    let* tipe = resolve_type table tipe meta in
    let field = RNode.VarDef.from_parts name tipe in
    return (field :: fields) in
  let* fields = List.fold_right fold_fn fields (Ok []) in
  return (RNode.Rec (name, fields, meta))

let resolve_def recur_fn scopes meta name expr =
  let* expr = recur_fn scopes expr in
  return (RNode.Def (name, expr, meta))

let resolve_fn recur_fn table scopes meta params rtype body =
  let fold_fn var vars =
    let* vars = vars in
    let (name, tipe) = PNode.VarDef.to_tuple var in
    let* t = resolve_type table tipe meta in
    let var = RNode.VarDef.from_parts name t in
    return (var :: vars) in
  let map_fn var =
    let (name, _) = RNode.VarDef.to_tuple var in
    RNode.VarDef.Name.to_string name in
  let params = List.fold_right fold_fn params (Ok []) in
  let* params = params in
  let param_names = List.map map_fn params in
  let fn_scope = Scope.of_list param_names in
  let scopes = fn_scope :: scopes in
  let* rtype = resolve_type table rtype meta in
  let* body = recur_fn scopes body in
  return (RNode.Fn (params, rtype, body, meta))

let resolve_if recur_fn scopes meta tst iff els =
  let* tst = recur_fn scopes tst in
  let* iff = recur_fn scopes iff in
  let* els = recur_fn scopes els in
  return (RNode.If (tst, iff, els, meta))

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
    | _ :: tl -> Ok (new_scope :: tl, binding)
  end

let resolve_let recur_fn scopes meta bindings body =
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
    let* body = recur_fn scopes body in
    return (RNode.Let (List.rev bindings, body, meta))
  end

let resolve_apply recur_fn scopes meta fn args =
  let fold_fn resolved arg =
    let* r = resolved in
    let* a = recur_fn scopes arg in
    return (a :: r) in
  let args = List.fold_left fold_fn (Ok []) args in
  let* args = args in
  let* fn = recur_fn scopes fn in
  return (RNode.Apply (fn, List.rev args, meta))

let check_cons_fields fields tipe metadata =
  match tipe with
  | Type.Rec (_, name, cons) -> begin
    let cons_fields = List.map fst cons in
    if (List.compare_lengths cons_fields fields) = 0 then
      let field_exists cons_fields field =
        if List.exists ((=) field) cons_fields then Ok field
        else
          let prefix = build_prefix metadata in
          Error (Cmpl_err.name_errors metadata prefix [
            sprintf "%s is not a valid binding" @@ Type.Name.to_string field
          ])
      in
      let fold_fn field fields =
        let* fields = fields in
        let* field = field_exists cons_fields field in
        return (field :: fields) in
      List.fold_right fold_fn fields (Ok [])
    else
      let prefix = build_prefix metadata in
      Error (Cmpl_err.name_errors metadata prefix [
        sprintf "Wrong number of bindings for constructor '%s'" @@ Type.Name.to_string name
      ])
  end
  | _ -> assert false

let resolve_exprs recur_fn scopes exprs =
  let fold_fn expr exprs =
    let* exprs = exprs in
    let* expr = recur_fn scopes expr in
    return (expr :: exprs) in
  List.fold_right fold_fn exprs (Ok [])

let resolve_cons recur_fn table scopes meta tipe bindings =
  let* tipe = resolve_type table tipe meta in
  let fields = List.map PNode.Binding.name bindings in
  let* fields = check_cons_fields fields tipe meta in
  let exprs = List.map PNode.Binding.expr bindings in
  let* exprs = resolve_exprs recur_fn scopes exprs in
  let bindings = List.map2 RNode.Binding.from_node fields exprs in
  return (RNode.Cons (tipe, bindings, meta))

let resolve_get table scopes record field =
  match record with
  | PNode.SymLit (name, meta) -> begin
    let* symlit = resolve_symlit table scopes meta name in
    return (RNode.Get (symlit, field, meta))
  end
  | _ -> assert false

let resolve_set recur_fn table scopes record field expr =
  match record with
  | PNode.SymLit (name, meta) -> begin
    let* symlit = resolve_symlit table scopes meta name in
    let* expr = recur_fn scopes expr in
    return (RNode.Set (symlit, field, expr, meta))
  end
  | _ -> assert false

let resolve_cast recur_fn table scopes meta tipe expr =
  let* tipe = resolve_type table tipe meta in
  let* expr = recur_fn scopes expr in
  return (RNode.Cast (tipe, expr, meta))

let resolve_node table node =
  let rec resolve' scopes = function
    | PNode.NumLit (n, meta) -> Ok (RNode.NumLit (n, meta))
    | PNode.StrLit (s, meta) -> Ok (RNode.StrLit (s, meta))
    | PNode.SymLit (name, meta) -> resolve_symlit table scopes meta name
    | PNode.Rec (name, fields, meta) -> resolve_rec table meta name fields
    | PNode.Def (var, expr, meta) -> resolve_def resolve' scopes meta var expr
    | PNode.Fn (params, rtype, body, meta) -> resolve_fn resolve' table scopes meta params rtype body
    | PNode.If (tst, iff, els, meta) -> resolve_if resolve' scopes meta tst iff els
    | PNode.Let (bindings, body, meta) -> resolve_let resolve' scopes meta bindings body
    | PNode.Apply (fn, args, meta) -> resolve_apply resolve' scopes meta fn args
    | PNode.Cons (tipe, bindings, meta) -> resolve_cons resolve' table scopes meta tipe bindings
    | PNode.Get (record, field, _) -> resolve_get table scopes record field
    | PNode.Set (record, field, expr, _) -> resolve_set resolve' table scopes record field expr
    | PNode.Cast (tipe, expr, meta) -> resolve_cast resolve' table scopes meta tipe expr in
  resolve' [] node

let resolve_nodes table nodes =
  let fold_fn node nodes =
    let* nodes = nodes in
    let* node = resolve_node table node in
    return (node :: nodes) in
  List.fold_right fold_fn nodes (Ok [])

let resolve table nodes =
  resolve_nodes table nodes
