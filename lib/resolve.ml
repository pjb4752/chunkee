open Printf
open Thwack.Extensions
open Thwack.Result

type t = Name.t Node.t

module Scope = Set.Make(String)

let ifndef_var modul vardef =
  let (name, t) = Node.VarDef.to_tuple vardef in
  let name_s = Node.VarDef.Name.to_string name in
  let name = Module.Var.Name.from_string name_s in
  if Module.var_exists modul name then
    Error (Cmpl_err.NameError (sprintf "var %s already defined" name_s))
  else
    match Type.from_node t with
    | Some t -> Ok (Module.define_var modul name t)
    | None ->
      let type_s = Node.TypeDef.to_string t in
      Error (Cmpl_err.NameError (sprintf "unknown type %s" type_s))

let define_var modul = function
  | Node.Def (var, _) -> ifndef_var modul var
  | _ -> Ok (modul)

let define_vars modul nodes =
  let fold_fn node modul =
    modul >>= fun m ->
    (define_var m node) >>= fun m ->
    return m in
  List.fold_right fold_fn nodes (Ok modul)

let undefined_name_error n =
  Error (Cmpl_err.NameError (sprintf "%s is undefined" n))

let resolve_module_name modul name =
  let name = Module.Var.Name.from_string name in
  if Module.var_exists modul name then Some name
  else None

let resolve_global_name table name =
  let name = Module.Var.Name.from_string name in
  match Symbol_table.find_module table Stdlib.global_name with
  | Some m ->
      if Module.var_exists m name then Some (m, name)
      else None
  | None -> None

let make_symlit modul m_name =
  let qual_name = Module.qual_name modul in
  let name = Name.Module (qual_name, m_name) in
  Ok (Node.SymLit name)

let is_qualified name =
  String.contains name '.'

let resolve_qualified_name table modul name =
  let bad_name_error n =
    Error (Cmpl_err.NameError (sprintf "unknown name format %s" n)) in
  let unknown_modul_error n =
    Error (Cmpl_err.NameError (sprintf "unknown module %s" n)) in
  match String.split_on_char '/' name with
  | module_name :: var_name :: [] -> begin
    let parts = String.split_on_char '.' module_name in
    let parts = List.map (Module.Name.from_string) parts in
    match List.rev parts with
    | name :: path_parts -> begin
      let path = Module.Path.from_list path_parts in
      let qual_name = Module.Qual_name.make path name in
      match Symbol_table.find_module table qual_name with
      | Some m -> begin
        match resolve_module_name m var_name with
        | Some name -> make_symlit m name
        | None -> undefined_name_error var_name
      end
      | None -> unknown_modul_error (Module.Qual_name.to_string qual_name)
    end
    | _ -> bad_name_error name
  end
  | _ -> bad_name_error name

let resolve_unqualified_name table modul name =
  match resolve_module_name modul name with
  | Some name -> make_symlit modul name
  | None -> begin
    match resolve_global_name table name with
    | Some (m, name) -> make_symlit m name
    | _ -> undefined_name_error name
  end

let resolve_name table modul scopes name =
  if List.exists (Scope.mem name) scopes then
    Ok (Node.SymLit (Name.Local name))
  else if is_qualified name then resolve_qualified_name table modul name
  else resolve_unqualified_name table modul name

let resolve_def recur_fn scopes var expr =
  (recur_fn scopes expr) >>= fun n ->
  return (Node.Def (var, n))

let resolve_fn recur_fn scopes params body =
  let map_fn var_def =
    let (name, _) = Node.VarDef.to_tuple var_def in
    Node.VarDef.Name.to_string name in
  let param_names = List.map map_fn params in
  let fn_scope = Scope.of_list param_names in
  let scopes = fn_scope :: scopes in
  (recur_fn scopes body) >>= fun n ->
  return (Node.Fn (params, n))

let resolve_if recur_fn scopes tst iff els =
  (recur_fn scopes tst) >>= fun t ->
  (recur_fn scopes iff) >>= fun i ->
  (recur_fn scopes els) >>= fun e ->
  return (Node.If (t, i, e))

let resolve_binding recur_fn scopes binding =
  let (name, expr) = Node.Binding.to_tuple binding in
  match recur_fn scopes expr with
  | Error e -> Error e
  | Ok expr -> begin
    (*TODO we should always create new scope for each binding*)
    let binding = Node.Binding.from_node name expr in
    let current_scope = List.hd_else scopes Scope.empty in
    let name = Node.Binding.Name.to_string name in
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
  | Ok (s, bs) -> begin
    (recur_fn s body) >>= fun b ->
    return (Node.Let (List.rev bs, b))
  end

let resolve_apply recur_fn scopes fn args =
  let fold_fn resolved arg =
    resolved >>= fun r ->
    (recur_fn scopes arg) >>= fun a ->
    return (a :: r) in
  let args = List.fold_left fold_fn (Ok []) args in
  args >>= fun a ->
  (recur_fn scopes fn) >>= fun f ->
  return (Node.Apply (f, List.rev a))

let resolve_cast recur_fn scopes tdef expr =
  (recur_fn scopes expr) >>= fun e ->
  return (Node.Cast (tdef, e))

let resolve_node table modul node =
  let rec resolve' scopes = function
    | Node.NumLit n -> Ok (Node.NumLit n)
    | Node.StrLit s -> Ok (Node.StrLit s)
    | Node.SymLit name -> resolve_name table modul scopes name
    | Node.Def (var, expr) -> resolve_def resolve' scopes var expr
    | Node.Fn (params, body) -> resolve_fn resolve' scopes params body
    | Node.If (tst, iff, els) -> resolve_if resolve' scopes tst iff els
    | Node.Let (bindings, body) -> resolve_let resolve' scopes bindings body
    | Node.Apply (fn, args) -> resolve_apply resolve' scopes fn args
    | Node.Cast (tdef, expr) -> resolve_cast resolve' scopes tdef expr in
  resolve' [] node

let resolve table modul nodes =
  let fold_fn node nodes =
    nodes >>= fun ns ->
    (resolve_node table modul node) >>= fun n ->
    return (n :: ns) in
  List.fold_right fold_fn nodes (Ok [])
