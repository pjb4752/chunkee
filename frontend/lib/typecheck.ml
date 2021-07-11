open Printf
open Common.Extensions
open Common.Extensions.Result
open Common.Extensions.Result.Syntax
open Names

module Form = Ast.Resolved_form

type t = (Type.t, Compile_error.t) result

module Scope = Map.Make(String)

let is_compatible expected actual =
  expected = actual || expected == Type.Any

let typecheck_local_name scopes name =
  match List.find_opt (Scope.mem name) scopes with
  | None -> assert false
  | Some s -> begin
      match Scope.find_opt name s with
      | None -> assert false
      | Some t -> Ok t
  end

let typecheck_module_name symbol_table module_name var_name =
  match Symbol_table.find_variable_type symbol_table module_name var_name with
  | None -> assert false
  | Some tipe -> Ok tipe

let typecheck_name symbol_table scopes name =
  match name with
  | Resolved_name.LocalName name -> typecheck_local_name scopes name
  | Resolved_name.ModuleName (module_name, name) -> typecheck_module_name symbol_table module_name name

let build_parameter_scope parameters =
  let build_parameter_tuples param processed_params =
    let* processed_params = processed_params in
    let (param_name, param_type) = Form.VarDef.to_tuple param in
    return ((param_name, param_type) :: processed_params) in
  let* parameter_tuples = List.fold_right build_parameter_tuples parameters (Ok []) in
  return (
    List.fold_right (fun (param_name, param_type) scope ->
      Scope.add param_name param_type scope
    ) parameter_tuples Scope.empty
  )

let typecheck_fn recursively_typecheck scopes metadata parameters return_type body_form =
  let actual_return_type =
    let* param_scope = build_parameter_scope parameters in
    let* return_type = recursively_typecheck (param_scope :: scopes) body_form in
    return return_type in
  match actual_return_type with
  | Error e -> Error e
  | Ok actual_return_type when return_type = actual_return_type -> Ok actual_return_type
  | Ok actual_return_type ->
      Error (Compile_error.type_errors metadata [
        sprintf "expected return type is %s, " @@ Type.inspect return_type;
        sprintf "but actual return type found is %s" @@ Type.inspect actual_return_type
      ])

let check_test_type metadata test_type =
  match test_type with
  | Type.Bool -> Ok Type.Bool
  | actual_type ->
      Error (Compile_error.type_errors metadata [
        "if test-exprs must evaulate to a boolean value, ";
        sprintf "instead received type of %s" @@ Type.inspect actual_type
      ])

let typecheck_test_form recursively_typecheck scopes metadata test_form =
  let* test_type = recursively_typecheck scopes test_form in
  return (check_test_type metadata test_type)

let typecheck_if_branches metadata if_type else_type =
  if if_type = else_type then Ok if_type
  else if if_type = Type.Any || else_type = Type.Any then Ok Type.Any
  else
    Error (Compile_error.type_errors metadata [
      sprintf "result of if-expr was %s, " @@ Type.inspect if_type;
      sprintf "which is incompatible with else-expr result type %s" @@ Type.inspect else_type
  ])

let typecheck_if recursively_typecheck scopes metadata test_form if_form else_form =
  let* _ = typecheck_test_form recursively_typecheck scopes metadata test_form in
  let* if_type = recursively_typecheck scopes if_form in
  let* else_type = recursively_typecheck scopes else_form in
  let* return_type = typecheck_if_branches metadata if_type else_type in
  return return_type

let typecheck_binding recursively_typecheck scopes binding =
  let (binding_name, binding_body) = Form.Binding.to_tuple binding in
  match recursively_typecheck scopes binding_body with
  | Error e -> Error e
  | Ok binding_type -> begin
    let (scope: 'a Scope.t) = Scope.add binding_name binding_type Scope.empty in
    Ok (scope :: scopes)
  end

let typecheck_let recursively_typecheck scopes bindings body_form =
  let rec typecheck_bindings scopes = function
    | [] -> Ok scopes
    | binding :: remaining_bindings -> begin
      match typecheck_binding recursively_typecheck scopes binding with
      | Error e -> Error e
      | Ok scopes -> typecheck_bindings scopes remaining_bindings
    end in
  let* scopes = typecheck_bindings scopes bindings in
  let* return_type = recursively_typecheck scopes body_form in
  return return_type

let typecheck_parameter_types metadata expected_types actual_types return_type =
  let typecheck_param_type index (expected, actual) =
    if is_compatible expected actual then None
    else
      let param_number = index + 1
      and expected_type = Type.inspect expected
      and actual_type = Type.inspect actual in
      Some [
        sprintf "function expected argument %d to be of type %s, " param_number expected_type;
        sprintf "instead received actual argument of type %s\n\t" actual_type
      ]
  in
  let combine_results errors = function
    | None -> errors
    | Some error -> errors @ error
  in
  let results = List.mapi typecheck_param_type @@ List.zip expected_types actual_types in
  let errors = List.fold_left combine_results [] results in
  if List.is_empty errors then Ok return_type
  else
    Error (Compile_error.type_errors metadata errors)

let typecheck_callable_arguments metadata defined_type argument_types =
  match defined_type with
  | Type.Function (expected_types, return_type) -> begin
    if List.compare_lengths expected_types argument_types = 0 then
      typecheck_parameter_types metadata expected_types argument_types return_type
    else
      Error (Compile_error.type_errors metadata [
        sprintf "function expected %d arguments, " @@ List.length expected_types;
        sprintf "but instead received %d" @@ List.length argument_types
      ])
  end
  | defined_type ->
      Error (Compile_error.type_errors metadata [
        sprintf "attempt to apply non-function type of %s" @@ Type.inspect defined_type
      ])

let typecheck_apply recursively_typecheck scopes metadata callable_form arguments =
  let typecheck_arguments argument checked_types =
    let* checked_types = checked_types in
    let* next_type = recursively_typecheck scopes argument in
    return (next_type :: checked_types) in
  let* argument_types = List.fold_right typecheck_arguments arguments (Ok []) in
  let* defined_type = recursively_typecheck scopes callable_form in
  let* return_type = typecheck_callable_arguments metadata defined_type argument_types in
  return return_type

(*TODO handle this more like function arguments*)
let compare_field_types metadata target_fields bound_field bound_type =
  match List.find_opt (fun target_field -> (fst target_field) = bound_field) target_fields with
  | Some (_, target_type) when is_compatible target_type bound_type -> Ok bound_type
  | Some (_, target_type) ->
      Error (Compile_error.type_errors metadata [
        sprintf "constructor expected type of %s, " @@ Type.inspect target_type;
        sprintf "but instead received type of %s" @@ Type.inspect bound_type
  ])
  | None -> assert false

let typecheck_cons recursively_typecheck scopes metadata target_type bindings =
  match target_type with
  | Type.Record (target_fields) -> begin
    let typecheck_field_types binding bound_types =
      let* bound_types = bound_types in
      let (bound_field, body_form) = Form.Binding.to_tuple binding in
      let* bound_type = recursively_typecheck scopes body_form in
      let* bound_type = compare_field_types metadata target_fields bound_field bound_type in
      return (bound_type :: bound_types) in
    let bound_types = List.fold_right typecheck_field_types bindings (Ok []) in
    let* _ = bound_types in return target_type
    end
  | _ -> assert false

let typecheck_record_type metadata target_type =
  match target_type with
  | Type.Record fields -> Ok fields
  | actual_type ->
      Error (Compile_error.type_errors metadata [
        "first arg to 'get' builtin must be record type, ";
        sprintf "instead received %s" @@ Type.inspect actual_type
  ])

let typecheck_record_field metadata target_fields field =
  match List.find_opt (fun (name, _) -> name = field) target_fields with
  | Some (_, target_type) -> Ok (target_type)
  | None ->
      Error (Compile_error.name_errors metadata [
        sprintf "record does not have field %s" field
      ])

let typecheck_get symbol_symbol_table scopes metadata target_form field =
  match target_form with
  | { Form.parsed = Form.Symbol name; _ } -> begin
    let* target_type = typecheck_name symbol_symbol_table scopes name in
    let* target_fields = typecheck_record_type metadata target_type in
    let* target_type = typecheck_record_field metadata target_fields field in
    return target_type
  end
  | _ -> assert false

let compare_set_type metadata target_type actual_type =
  if is_compatible target_type actual_type then Ok actual_type
  else
    Error (Compile_error.type_errors metadata [
      sprintf "builtin 'set' expected type of %s, " @@ Type.inspect target_type;
      sprintf "but instead received type of %s" @@ Type.inspect actual_type
    ])

let typecheck_set recursively_typecheck symbol_symbol_table scopes metadata target_form field body_form =
  match target_form with
  | { Form.parsed = Form.Symbol name; _ } -> begin
    let* target_type = typecheck_name symbol_symbol_table scopes name in
    let* target_fields = typecheck_record_type metadata target_type in
    let* target_type = typecheck_record_field metadata target_fields field in
    let* actual_type = recursively_typecheck scopes body_form in
    let* _ = compare_set_type metadata target_type actual_type in
    return Type.Unit
  end
  | _ -> assert false

let typecheck_cast recursively_typecheck scopes target_type body_form =
  let* _ = recursively_typecheck scopes body_form in
  return target_type

let typecheck_form symbol_table form =
  let rec recursively_typecheck scopes (form: Form.t) =
    let metadata = form.metadata in
    match form.parsed with
    | Form.Number _ ->
        Ok Type.Number
    | Form.String _ ->
        Ok Type.String
    | Form.Symbol name ->
        typecheck_name symbol_table scopes name
    | Form.Fn { parameters; return_type; body_form } ->
        typecheck_fn recursively_typecheck scopes metadata parameters return_type body_form
    | Form.If { test_form; if_form; else_form } ->
        typecheck_if recursively_typecheck scopes metadata test_form if_form else_form
    | Form.Let { bindings; body_form } ->
        typecheck_let recursively_typecheck scopes bindings body_form
    | Form.Apply { callable_form; arguments } ->
        typecheck_apply recursively_typecheck scopes metadata callable_form arguments
    | Form.Cons { target_type; bindings } ->
        typecheck_cons recursively_typecheck scopes metadata target_type bindings
    | Form.Get { target_form; field } ->
        typecheck_get symbol_table scopes metadata target_form field
    | Form.Set { target_form; field; body_form } ->
        typecheck_set recursively_typecheck symbol_table scopes metadata target_form field body_form
    | Form.Cast { target_type; body_form } ->
        typecheck_cast recursively_typecheck scopes target_type body_form
    | Form.Type _ -> assert false
    | Form.Def _ -> assert false
  in
  recursively_typecheck [] form
