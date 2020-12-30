open Printf
open Common.Extensions
open Common.Extensions.Result
open Common.Extensions.Result.Syntax

module Scope = Set.Make(String)

module PNode = Ast.Parsed_node
module RNode = Ast.Resolved_node

type t = (RNode.t, Cmpl_err.t) result

let build_error_prefix { Metadata.line_num; char_num; _ } =
  sprintf "in expression at %d:%d" line_num char_num

let resolve_symbol symbol_table scopes metadata name =
  let name = Symbol_table.resolve_name symbol_table (fun name ->
    List.exists (Scope.mem name) scopes) name
  in
  match name with
  | Ok name -> Ok { RNode.metadata; parsed = RNode.Symbol name }
  | Error error ->
      let prefix = build_error_prefix metadata in
      Error (Cmpl_err.name_errors metadata prefix [
        Symbol_table.err_string error
      ])

let resolve_type symbol_table metadata parsed_type =
  match Symbol_table.resolve_type symbol_table parsed_type with
  | Ok resolved_type -> Ok resolved_type
  | Error error ->
      let prefix = build_error_prefix metadata in
      Error (Cmpl_err.name_errors metadata prefix [
        Symbol_table.err_string error
      ])

let resolve_def recur_fn scopes metadata name body_node =
  let* body_node = recur_fn scopes body_node in
  return { RNode.metadata; parsed = RNode.Def { name; body_node } }

let resolve_function recur_fn symbol_table scopes metadata parameters return_type body_node =
  let resolve_vars parsed_var resolved_vars =
    let* resolved_vars = resolved_vars in
    let (name, parsed_type) = PNode.VarDef.to_tuple parsed_var in
    let* resolved_type = resolve_type symbol_table metadata parsed_type in
    let resolved_var = RNode.VarDef.from_parts name resolved_type in
    return (resolved_var :: resolved_vars) in
  let extract_var_name resolved_var =
    RNode.VarDef.to_tuple resolved_var |> fst |> Identifier.to_string in
  let* parameters = List.fold_right resolve_vars parameters (Ok []) in
  let param_names = List.map extract_var_name parameters in
  let function_scope = Scope.of_list param_names in
  let scopes = function_scope :: scopes in
  let* return_type = resolve_type symbol_table metadata return_type in
  let* body_node = recur_fn scopes body_node in
  return { RNode.metadata; parsed = RNode.Fn { parameters; return_type; body_node } }

let resolve_if recur_fn scopes metadata test_node if_node else_node =
  let* test_node = recur_fn scopes test_node in
  let* if_node = recur_fn scopes if_node in
  let* else_node = recur_fn scopes else_node in
  return { RNode.metadata; parsed = RNode.If { test_node; if_node; else_node } }

let resolve_binding recur_fn scopes binding =
  let (name, node) = PNode.Binding.to_tuple binding in
  match recur_fn scopes node with
  | Error e -> Error e
  | Ok node -> begin
    (*TODO we should always create new scope for each binding*)
    let binding = RNode.Binding.from_node name node in
    let current_scope = List.hd_else scopes Scope.empty in
    let name = Identifier.to_string name in
    let updated_scope = Scope.add name current_scope in
    match scopes with
    | [] -> Ok ([updated_scope], binding)
    | _ :: previous_scopes -> Ok (updated_scope :: previous_scopes, binding)
  end

let resolve_let recur_fn scopes metadata bindings body_node =
  let rec resolve_bindings' scopes resolved = function
    | [] -> Ok (scopes, resolved)
    | binding :: bindings -> begin
      match resolve_binding recur_fn scopes binding with
      | Error e -> Error e
      | Ok (updated_scopes, binding) -> resolve_bindings' updated_scopes (binding :: resolved) bindings
    end in
  match resolve_bindings' scopes [] bindings with
  | Error e -> Error e
  | Ok (scopes, bindings) -> begin
    let* body_node = recur_fn scopes body_node in
    return { RNode.metadata; parsed = RNode.Let { bindings = List.rev bindings; body_node } }
  end

let resolve_apply recur_fn scopes metadata callable_node arguments =
  let resolve_arguments resolved argument =
    let* resolved = resolved in
    let* argument = recur_fn scopes argument in
    return (argument :: resolved) in
  let* arguments = List.fold_left resolve_arguments (Ok []) arguments in
  let* callable_node = recur_fn scopes callable_node in
  return { RNode.metadata; parsed = RNode.Apply { callable_node; arguments = List.rev arguments } }

let check_record_fields metadata record_type given_names =
  match record_type with
  | Type.Record defined_fields -> begin
    let defined_names = List.map fst defined_fields in
    if (List.compare_lengths defined_names given_names) = 0 then
      let field_exists defined_names given_name =
        if List.exists ((=) given_name) defined_names then Ok given_name
        else
          let prefix = build_error_prefix metadata in
          Error (Cmpl_err.name_errors metadata prefix [
            sprintf "%s is not a valid record field" @@ Identifier.to_string given_name
          ])
      in
      let check_fields_exist given_name existing_names =
        let* existing_names = existing_names in
        let* existing_name = field_exists defined_names given_name in
        return (existing_name :: existing_names) in
      List.fold_right check_fields_exist given_names (Ok [])
    else
      let prefix = build_error_prefix metadata in
      Error (Cmpl_err.name_errors metadata prefix [
        sprintf "Wrong number of fields for given for record"
      ])
  end
  | _ -> assert false

let resolve_record_expressions recur_fn scopes expression_nodes =
  let resolve_expressions expression_node resolved_nodes =
    let* resolved_nodes = resolved_nodes in
    let* resolved_node = recur_fn scopes expression_node in
    return (resolved_node :: resolved_nodes) in
  List.fold_right resolve_expressions expression_nodes (Ok [])

let resolve_cons recur_fn symbol_table scopes metadata target_type fields =
  let* target_type = resolve_type symbol_table metadata target_type in
  let given_fields = List.map PNode.Binding.name fields in
  let* existing_fields = check_record_fields metadata target_type given_fields in
  let field_expressions = List.map PNode.Binding.expr fields in
  let* resolved_expressions = resolve_record_expressions recur_fn scopes field_expressions in
  let resolved_fields = List.map2 RNode.Binding.from_node existing_fields resolved_expressions in
  return { RNode.metadata; parsed = RNode.Cons { target_type; bindings = resolved_fields } }

let resolve_get symbol_table scopes target_node field =
  match target_node with
  | { PNode.metadata; parsed = PNode.Symbol name } -> begin
    let* target_node = resolve_symbol symbol_table scopes metadata name in
    return { RNode.metadata; parsed = RNode.Get { target_node; field } }
  end
  | _ -> assert false

let resolve_set recur_fn symbol_table scopes target_node field body_node =
  match target_node with
  | { PNode.metadata; parsed = PNode.Symbol name } -> begin
    let* target_node = resolve_symbol symbol_table scopes metadata name in
    let* body_node = recur_fn scopes body_node in
    return { RNode.metadata; parsed = RNode.Set { target_node; field; body_node } }
  end
  | _ -> assert false

let resolve_cast recur_fn symbol_table scopes metadata target_type body_node =
  let* target_type = resolve_type symbol_table metadata target_type in
  let* body_node = recur_fn scopes body_node in
  return { RNode.metadata; parsed = RNode.Cast { target_type; body_node } }

let resolve_node symbol_table node =
  let rec resolve' scopes (node: PNode.t) =
    let metadata = node.metadata in
    match node.parsed with
    | PNode.NumLit value ->
        Ok { RNode.metadata; parsed = RNode.NumLit value }
    | PNode.StrLit value ->
        Ok { RNode.metadata; parsed = RNode.StrLit value }
    | PNode.Symbol value ->
        resolve_symbol symbol_table scopes metadata value
    | PNode.Def { name; body_node }->
        resolve_def resolve' scopes metadata name body_node
    | PNode.Fn { parameters; return_type; body_node } ->
        resolve_function resolve' symbol_table scopes metadata parameters return_type body_node
    | PNode.If { test_node; if_node; else_node } ->
        resolve_if resolve' scopes metadata test_node if_node else_node
    | PNode.Let { bindings; body_node } ->
        resolve_let resolve' scopes metadata bindings body_node
    | PNode.Apply { callable_node; arguments }->
        resolve_apply resolve' scopes metadata callable_node arguments
    | PNode.Cons { target_type; bindings } ->
        resolve_cons resolve' symbol_table scopes metadata target_type bindings
    | PNode.Get { target_node; field } ->
        resolve_get symbol_table scopes target_node field
    | PNode.Set { target_node; field; body_node } ->
        resolve_set resolve' symbol_table scopes target_node field body_node
    | PNode.Cast { target_type; body_node } ->
        resolve_cast resolve' symbol_table scopes metadata target_type body_node
    | PNode.Type _ -> assert false in
  resolve' [] node

let inspect node =
  Result.inspect node RNode.inspect Cmpl_err.to_string
