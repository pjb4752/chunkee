open Printf
open Common.Extensions
open Common.Extensions.Result
open Common.Extensions.Result.Syntax

module Semantic_form = Ast.Semantic_form
module Resolved_form = Ast.Resolved_form

module Scope = Set.Make(String)

module Result = struct
  type t = (Resolved_form.t, Compile_error.t) result

  let inspect result =
    Result.inspect Resolved_form.inspect Compile_error.to_string result
end

let create_name_error position message_fragments =
  Error (Compile_error.create_name_error position [message_fragments])

let resolve_symbol symbol_table scopes position name =
  let name = Symbol_table.resolve_name symbol_table (fun name ->
    List.exists (Scope.mem name) scopes) name
  in
  match name with
  | Error error -> create_name_error position @@ Symbol_table.err_string error
  | Ok name -> Ok (Resolved_form.create_symbol position name)

let resolve_type symbol_table position parsed_type =
  match Symbol_table.resolve_type symbol_table parsed_type with
  | Error error -> create_name_error position @@ Symbol_table.err_string error
  | Ok resolved_type -> Ok resolved_type

let resolve_def_form recursively_resolve scopes position name body_form =
  let* body_form = recursively_resolve scopes body_form in
  return (Resolved_form.create_def position name body_form)

let resolve_function_form recursively_resolve symbol_table scopes position parameters return_type body_form =
  let resolve_parameter parameter =
    let (name, parsed_type) = Semantic_form.Parameter.to_tuple parameter in
    let* resolved_type = resolve_type symbol_table position parsed_type in
    return (Resolved_form.Parameter.create name resolved_type)
  in
  let* parameters = List.bind_right resolve_parameter parameters in
  let parameter_names = List.map Resolved_form.Parameter.name parameters in
  let function_scope = Scope.of_list parameter_names in
  let scopes = function_scope :: scopes in
  let* return_type = resolve_type symbol_table position return_type in
  let* body_form = recursively_resolve scopes body_form in
  return (Resolved_form.create_fn position parameters return_type body_form)

let resolve_if_form recursively_resolve scopes position test_form if_form else_form =
  let* test_form = recursively_resolve scopes test_form in
  let* if_form = recursively_resolve scopes if_form in
  let* else_form = recursively_resolve scopes else_form in
  return (Resolved_form.create_if position test_form if_form else_form)

let resolve_binding recursively_resolve scopes binding =
  let (name, form) = Semantic_form.Binding.to_tuple binding in
  match recursively_resolve scopes form with
  | Error e -> Error e
  | Ok form -> begin
    (*TODO we should always create new scope for each binding*)
    let binding = Resolved_form.Binding.create name form in
    let current_scope = List.hd_else scopes Scope.empty in
    let updated_scope = Scope.add name current_scope in
    match scopes with
    | [] -> Ok ([updated_scope], binding)
    | _ :: previous_scopes -> Ok (updated_scope :: previous_scopes, binding)
  end

let resolve_let_form recursively_resolve scopes position bindings body_form =
  let rec resolve_bindings' scopes resolved = function
    | [] -> Ok (scopes, resolved)
    | binding :: bindings -> begin
      match resolve_binding recursively_resolve scopes binding with
      | Error e -> Error e
      | Ok (updated_scopes, binding) -> resolve_bindings' updated_scopes (binding :: resolved) bindings
    end in
  match resolve_bindings' scopes [] bindings with
  | Error e -> Error e
  | Ok (scopes, bindings) -> begin
    let* body_form = recursively_resolve scopes body_form in
    return (Resolved_form.create_let position (List.rev bindings) body_form)
  end

let resolve_apply_form recursively_resolve scopes position callable_form arguments =
  let* arguments = List.bind_left (fun argument -> recursively_resolve scopes argument) arguments in
  let* callable_form = recursively_resolve scopes callable_form in
  return (Resolved_form.create_apply position callable_form @@ List.rev arguments)

let check_record_fields position record_type given_names =
  match record_type with
  | Type.Record defined_fields -> begin
    let defined_names = List.map fst defined_fields in
    if (List.compare_lengths defined_names given_names) = 0 then
      let field_exists defined_names given_name =
        if List.exists ((=) given_name) defined_names then Ok given_name
        else create_name_error position @@ sprintf "%s is not a valid record field" given_name
      in
      List.bind_right (fun given_name -> field_exists defined_names given_name) given_names
    else create_name_error position "Wrong number of fields for given for record"
  end
  | _ -> assert false

let resolve_record_expressions recursively_resolve scopes expression_forms =
  List.bind_right (fun expression_form -> recursively_resolve scopes expression_form) expression_forms

let resolve_cons_form recursively_resolve symbol_table scopes position target_type fields =
  let* target_type = resolve_type symbol_table position target_type in
  let given_fields = List.map Semantic_form.Binding.name fields in
  let* existing_fields = check_record_fields position target_type given_fields in
  let field_expressions = List.map Semantic_form.Binding.expr fields in
  let* resolved_expressions = resolve_record_expressions recursively_resolve scopes field_expressions in
  let resolved_fields = List.map2 Resolved_form.Binding.create existing_fields resolved_expressions in
  return (Resolved_form.create_cons position target_type resolved_fields)

let resolve_get_form symbol_table scopes target_form field =
  match target_form with
  | { Semantic_form.position; parsed = Semantic_form.Symbol name } -> begin
    let* target_form = resolve_symbol symbol_table scopes position name in
    return (Resolved_form.create_get position target_form field)
  end
  | _ -> assert false

let resolve_set_form recursively_resolve symbol_table scopes target_form field body_form =
  match target_form with
  | { Semantic_form.position; parsed = Semantic_form.Symbol name } -> begin
    let* target_form = resolve_symbol symbol_table scopes position name in
    let* body_form = recursively_resolve scopes body_form in
    return (Resolved_form.create_set position target_form field body_form)
  end
  | _ -> assert false

let resolve_cast_form recursively_resolve symbol_table scopes position target_type body_form =
  let* target_type = resolve_type symbol_table position target_type in
  let* body_form = recursively_resolve scopes body_form in
  return (Resolved_form.create_cast position target_type body_form)

let resolve_identifiers symbol_table form =
  let rec resolve' scopes (form: Semantic_form.t) =
    let position = form.position in
    match form.parsed with
    | Semantic_form.Number value ->
        Ok (Resolved_form.create_number position value)
    | Semantic_form.String value ->
        Ok (Resolved_form.create_string position value)
    | Semantic_form.Symbol value ->
        resolve_symbol symbol_table scopes position value
    | Semantic_form.Def { name; body_form }->
        resolve_def_form resolve' scopes position name body_form
    | Semantic_form.Fn { parameters; return_type; body_form } ->
        resolve_function_form resolve' symbol_table scopes position parameters return_type body_form
    | Semantic_form.If { test_form; if_form; else_form } ->
        resolve_if_form resolve' scopes position test_form if_form else_form
    | Semantic_form.Let { bindings; body_form } ->
        resolve_let_form resolve' scopes position bindings body_form
    | Semantic_form.Apply { callable_form; arguments }->
        resolve_apply_form resolve' scopes position callable_form arguments
    | Semantic_form.Cons { target_type; bindings } ->
        resolve_cons_form resolve' symbol_table scopes position target_type bindings
    | Semantic_form.Get { target_form; field } ->
        resolve_get_form symbol_table scopes target_form field
    | Semantic_form.Set { target_form; field; body_form } ->
        resolve_set_form resolve' symbol_table scopes target_form field body_form
    | Semantic_form.Cast { target_type; body_form } ->
        resolve_cast_form resolve' symbol_table scopes position target_type body_form
    in
  resolve' [] form
