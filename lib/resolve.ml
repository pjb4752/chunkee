open Printf
open Thwack.Extensions
open Thwack.Result

module Scope = Set.Make(String)

module Define = struct
  module Node = Ast.Parsed_node

  type t = (Module.t, Cmpl_err.t) result

  let ifndef_var modul name =
    let name_s = Node.Name.to_string name in
    let name = Var.Name.from_string name_s in
    if Module.var_exists modul name then
      Error (Cmpl_err.NameError (sprintf "var %s already defined" name_s))
    else
      Ok (Module.declare_var modul name)

  let ifndef_rec modul tname =
    if Module.type_exists modul tname then
      let name_s = Type.Name.to_string tname in
      Error (Cmpl_err.NameError (sprintf "type %s already defined" name_s))
    else
      let m_name = Module.name modul in
      Ok (Module.define_type modul (Type.Rec (m_name, tname)))

  let node_define modul = function
    | Node.Def (name, _) -> ifndef_var modul name
    | Node.Rec (name, _) -> ifndef_rec modul name
    | _ -> Ok modul

  let define modul nodes =
    let fold_fn node modul =
      modul >>= fun m ->
      let new_modul = node_define m node in
      new_modul >>= fun m ->
      return m in
    List.fold_right fold_fn nodes (Ok modul)
end

module Resolve = struct
  module PNode = Ast.Parsed_node
  module RNode = Ast.Resolved_node

  type s = (RNode.t, Cmpl_err.t) result
  type t = (RNode.t list, Cmpl_err.t) result

  let undefined_name_error n =
    Error (Cmpl_err.NameError (sprintf "%s is undefined" n))

  let resolve_module_name modul name =
    let name = Var.Name.from_string name in
    if Module.var_exists modul name then Some name
    else None

  let resolve_pervasive_name table name =
    let name = Var.Name.from_string name in
    let pervasive = Symbol_table.pervasive_module table in
    if Module.var_exists pervasive name then Some (pervasive, name)
    else None

  let is_qualified name = String.contains name '.'

  let make_symlit modul m_name =
    let mod_name = Module.name modul in
    let name = Name.Var.Module (mod_name, m_name) in
    Ok (RNode.SymLit name)

  let resolve_qualified_name table modul name =
    let bad_name_error n =
      Error (Cmpl_err.NameError (sprintf "unknown name format %s" n)) in
    let unknown_modul_error n =
      Error (Cmpl_err.NameError (sprintf "unknown module %s" n)) in
    match String.split_on_char '/' name with
    | module_name :: var_name :: [] -> begin
      let parts = String.split_on_char '.' module_name in
      let parts = List.map (Mod_name.Name.from_string) parts in
      match List.rev parts with
      | name :: path_parts -> begin
        let path = Mod_name.Path.from_list path_parts in
        let mod_name = Mod_name.make path name in
        match Symbol_table.find_module table mod_name with
        | Some m -> begin
          match resolve_module_name m var_name with
          | Some name -> make_symlit m name
          | None -> undefined_name_error var_name
        end
        | None -> unknown_modul_error (Mod_name.to_string mod_name)
      end
      | _ -> bad_name_error name
    end
    | _ -> bad_name_error name

  let resolve_unqualified_name table modul name =
    match resolve_module_name modul name with
    | Some name -> make_symlit modul name
    | None -> begin
      match resolve_pervasive_name table name with
      | Some (m, name) -> make_symlit m name
      | _ -> undefined_name_error name
    end

  let resolve_name table modul scopes name =
    let name = Ast.Var_ref.to_string name in
    if List.exists (Scope.mem name) scopes then
      Ok (RNode.SymLit (Name.Var.Local name))
    else if is_qualified name then resolve_qualified_name table modul name
    else resolve_unqualified_name table modul name

  let resolve_module_type modul tipe =
    let name = Type.Name.from_string tipe in
    Module.find_type modul name

  let resolve_qualified_type table modul tipe =
    let bad_name_error n =
      Error (Cmpl_err.NameError (sprintf "unknown name format %s" n)) in
    let unknown_modul_error n =
      Error (Cmpl_err.NameError (sprintf "unknown module %s" n)) in
    match String.split_on_char '/' tipe with
    | module_name :: type_name :: [] -> begin
      let parts = String.split_on_char '.' module_name in
      let parts = List.map (Mod_name.Name.from_string) parts in
      match List.rev parts with
      | name :: path_parts -> begin
        let path = Mod_name.Path.from_list path_parts in
        let mod_name = Mod_name.make path name in
        match Symbol_table.find_module table mod_name with
        | Some m -> begin
          match resolve_module_type m type_name with
          | Some tipe -> Ok tipe
          | None -> undefined_name_error type_name
        end
        | None -> unknown_modul_error (Mod_name.to_string mod_name)
      end
      | _ -> bad_name_error tipe
    end
    | _ -> bad_name_error tipe

  let resolve_unqualified_type modul tipe =
    match Type.find_builtin tipe with
    | Some tipe -> Ok tipe
    | None -> begin
      match resolve_module_type modul tipe with
      | Some tipe -> Ok tipe
      | None -> undefined_name_error tipe
    end

  let resolve_type table modul = function
    | Type_ref.StrType t when is_qualified t ->
        resolve_qualified_type table modul t
    | Type_ref.StrType t -> resolve_unqualified_type modul t
    | Type_ref.FnType ts -> Ok (Type.Num)

  let resolve_rec table modul scopes name fields =
    let fold_fn field fields =
      fields >>= fun fields ->
      let (name, tipe) = PNode.VarDef.to_tuple field in
      (resolve_type table modul tipe) >>= fun t ->
      let field = RNode.VarDef.from_parts name t in
      return (field :: fields) in
    let fields = List.fold_right fold_fn fields (Ok []) in
    fields >>= fun fields ->
    return (RNode.Rec (name, fields))

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
    let vars = List.fold_right fold_fn params (Ok []) in
    vars >>= fun v ->
      let param_names = List.map map_fn v in
      let fn_scope = Scope.of_list param_names in
      let scopes = fn_scope :: scopes in
      (recur_fn scopes body) >>= fun n ->
      return (RNode.Fn (v, n))

  let resolve_if recur_fn scopes tst iff els =
    (recur_fn scopes tst) >>= fun t ->
    (recur_fn scopes iff) >>= fun i ->
    (recur_fn scopes els) >>= fun e ->
    return (RNode.If (t, i, e))

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
    | Ok (s, bs) -> begin
      (recur_fn s body) >>= fun b ->
      return (RNode.Let (List.rev bs, b))
    end

  let resolve_apply recur_fn scopes fn args =
    let fold_fn resolved arg =
      resolved >>= fun r ->
      (recur_fn scopes arg) >>= fun a ->
      return (a :: r) in
    let args = List.fold_left fold_fn (Ok []) args in
    args >>= fun a ->
    (recur_fn scopes fn) >>= fun f ->
    return (RNode.Apply (f, List.rev a))

  let resolve_cast recur_fn table modul scopes tdef expr =
    (resolve_type table modul tdef) >>= fun t ->
    (recur_fn scopes expr) >>= fun e ->
    return (RNode.Cast (t, e))

  let resolve_node table modul node =
    let rec resolve' scopes = function
      | PNode.NumLit n -> Ok (RNode.NumLit n)
      | PNode.StrLit s -> Ok (RNode.StrLit s)
      | PNode.SymLit name -> resolve_name table modul scopes name
      | PNode.Rec (name, fields) -> resolve_rec table modul scopes name fields
      | PNode.Def (var, expr) -> resolve_def resolve' scopes var expr
      | PNode.Fn (params, body) -> resolve_fn resolve' table modul scopes params body
      | PNode.If (tst, iff, els) -> resolve_if resolve' scopes tst iff els
      | PNode.Let (bindings, body) -> resolve_let resolve' scopes bindings body
      | PNode.Apply (fn, args) -> resolve_apply resolve' scopes fn args
      | PNode.Cast (tdef, expr) -> resolve_cast resolve' table modul scopes tdef expr in
    resolve' [] node

  let resolve_names table modul nodes =
    let fold_fn node nodes =
      nodes >>= fun ns ->
      (resolve_node table modul node) >>= fun n ->
      return (n :: ns) in
    List.fold_right fold_fn nodes (Ok [])
end
