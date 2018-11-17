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

let check_cons_fields fields tipe =
  match tipe with
  | Type.Rec (_, name, cons) -> begin
    let cons_fields = List.map fst cons in
    if (List.compare_lengths cons_fields fields) = 0 then
      let field_exists cons_fields field =
        if List.exists ((=) field) cons_fields then Ok field
        else
          let field = Type.Name.to_string field in
          let message = sprintf "%s is not a valid binding" field in
          Error (Cmpl_err.NameError message) in
      let fold_fn field fields =
        fields >>= fun fields ->
        (field_exists cons_fields field) >>= fun field ->
        return (field :: fields) in
      List.fold_right fold_fn fields (Ok [])
    else
      let message = sprintf
        "Wrong number of bindings for constructor '%s'"
        (Type.Name.to_string name) in
      Error (Cmpl_err.NameError message)
  end
  | _ -> assert false

let resolve_exprs recur_fn scopes exprs =
  let fold_fn expr exprs =
    exprs >>= fun exprs ->
    (recur_fn scopes expr) >>= fun expr ->
    return (expr :: exprs) in
  List.fold_right fold_fn exprs (Ok [])

let resolve_cons recur_fn table scopes tipe bindings =
  (Symbol_table.resolve_type table tipe) >>= fun tipe ->
  let fields = List.map PNode.Binding.name bindings in
  (check_cons_fields fields tipe) >>= fun fields ->
  let exprs = List.map PNode.Binding.expr bindings in
  (resolve_exprs recur_fn scopes exprs) >>= fun exprs ->
  let bindings = List.map2 RNode.Binding.from_node fields exprs in
  return (RNode.Cons (tipe, bindings))

let resolve_get table scopes record field =
  match record with
  | PNode.SymLit name -> begin
    (resolve_symlit table scopes name) >>= fun symlit ->
    return (RNode.Get (symlit, field))
  end
  | _ -> assert false

let resolve_set recur_fn table scopes record field expr =
  match record with
  | PNode.SymLit name -> begin
    (resolve_symlit table scopes name) >>= fun symlit ->
    (recur_fn scopes expr) >>= fun expr ->
    return (RNode.Set (symlit, field, expr))
  end
  | _ -> assert false

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
    | PNode.Cons (tipe, bindings) ->
        resolve_cons resolve' table scopes tipe bindings
    | PNode.Get (record, field) -> resolve_get table scopes record field
    | PNode.Set (record, field, expr) ->
        resolve_set resolve' table scopes record field expr
    | PNode.Cast (tipe, expr) ->
        resolve_cast resolve' table scopes tipe expr in
  resolve' [] node

let resolve_nodes table nodes =
  let fold_fn node nodes =
    nodes >>= fun nodes ->
    (resolve_node table node) >>= fun node ->
    return (node :: nodes) in
  List.fold_right fold_fn nodes (Ok [])

let resolve table nodes =
  resolve_nodes table nodes
