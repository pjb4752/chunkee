open Printf
open Common.Extensions
open Common.Extensions.Result
open Common.Extensions.Result.Syntax
open Names

module Form = Ast.Semantic_form

module Result = struct
  type t = (Form.t, Compile_error.t) result

  let inspect result =
    Result.inspect Form.inspect Compile_error.to_string result
end

let parse_qualified_name position name =
  match String.split_on_char '/' name with
  | module_name :: object_name :: [] -> begin
    let module_segments = String.split_on_char '.' module_name in
    let module_segments = List.map (Module_name.Segment.from_string) module_segments in
    match List.rev module_segments with
    | basename :: segments -> begin
      let module_path = Module_name.Path.from_segments segments in
      let module_name = Module_name.from_path_and_base module_path basename in
      Ok (Unresolved_name.QualifiedName (module_name, object_name))
    end
    | _ ->
        Error (Compile_error.parse_errors position [
          "qualified names must contain a full path to a module";
          "\n\tplease use the correct form foo/bar.baz instead of foo/baz"
      ])
  end
  | _ ->
      Error (Compile_error.parse_errors position [
        "qualified names must contain both module tree and path information";
        "\n\tplease use the correct form foo/bar.baz instead of bar.baz"
    ])

let parse_name_expr position name =
  if String.contains name '.' then parse_qualified_name position name
  else Ok (Unresolved_name.UnqualifiedName name)

let parse_type_list parse_type_expr' forms_to_parse =
  List.fold_right (fun type_form parsed_types ->
    let* parsed_types = parsed_types in
    let* next_type = parse_type_expr' type_form in
    return (next_type :: parsed_types)) forms_to_parse (Ok [])

let invalid_type_error position =
  Error (Compile_error.parse_errors position [
    "expected a valid type expression, but did not find one";
    "\n\tplease use a correct simple or aggregate type."
  ])

let rec parse_type_expr { Source_form.position; value } =
  match value with
  | Symbol value -> begin
    let* parsed_type = parse_name_expr position value in
    return (Type_expression.SimpleType parsed_type)
  end
  | Vector forms -> begin
   match parse_type_list parse_type_expr forms with
   | Error e -> Error e
   | Ok [] ->
       Error (Compile_error.parse_errors position [
        "aggregate type expressions must contain 1 or more types, instead found an empty expression";
        "\n\tplease use the correct form [type1 type2 type3]"
     ])
   | Ok parsed_types -> Ok (Type_expression.CompoundType parsed_types)
  end
  | _ -> invalid_type_error position

let parse_var_def = function
  | { Source_form.value = Source_form.Symbol name; _ } :: type_form :: [] -> begin
    let* parsed_type = parse_type_expr type_form in
    return (name, parsed_type)
  end
  | first_form :: _ -> begin
    Error (Compile_error.parse_errors first_form.position [
      "variable definitions must be pairs of a name and a type";
      "\n\tplease use the correct form [name type]"
    ])
  end
  | _ -> assert false

let is_const_literal { Source_form.value; _ } =
  match value with
  | Source_form.Number _ | Source_form.String _ -> true
  | Source_form.List ({ value = Source_form.Symbol "fn"; _ } :: _) -> true
  | _ -> false

let parse_def recursively_parse position = function
  | { Source_form.value = Source_form.Symbol name; _ } :: body_expr :: [] when is_const_literal body_expr ->
      let* body_form = recursively_parse body_expr in
      return { Form.position; parsed = Form.Def { name; body_form } }
  | { Source_form.value = Source_form.Symbol _; _ } :: _ :: [] -> begin
    Error (Compile_error.parse_errors position [
      "top-level definitions must evaluate to a constant value";
      "\n\tplease use the correct form (def name <constant-expression>)"
    ])
  end
  | _ -> begin
    Error (Compile_error.parse_errors position [
      "top-level variable definitions must provide a name and an expr";
      "\n\tplease use the correct form (def name <constant-expression>)"
    ])
  end

let parse_get position = function
  | [{ Source_form.position; value = Source_form.Symbol target; _ }; { value = Source_form.Symbol field; _ }] ->
    let* target = parse_name_expr position target in
    let target_form = { Form.position; parsed = Form.Symbol target } in
    return { Form.position; parsed = Form.Get { target_form; field } }
  | _ -> begin
    Error (Compile_error.parse_errors position [
      "get expression must provide the record and field name";
      "\n\tplease use the correct form (get record field)"
    ])
  end

let parse_set recursively_parse position = function
  | [{ Source_form.position; value = Source_form.Symbol target; _ }; { value = Source_form.Symbol field; _ }; body] ->
    let* target = parse_name_expr position target in
    let* body_form = recursively_parse body in
    let target_form = { Form.position; parsed = Form.Symbol target } in
    return { Form.position; parsed = Form.Set { target_form; field; body_form } }
  | _ -> begin
    Error (Compile_error.parse_errors position [
      "set expression must provide record and field names, and an expression";
      "\n\tplease use the correct form (set! record field expression)"
    ])
  end

let parse_function_parameters position parameters =
  let parse_parameter (name, param_type) parsed_params =
    let* parsed_params = parsed_params in
    let* (name, param_type) = parse_var_def [name; param_type] in
    let param = Form.VarDef.from_parts name param_type in
    return (param :: parsed_params) in
  if (List.length parameters mod 2) = 0 then
    List.fold_right parse_parameter (List.as_pairs parameters) (Ok [])
  else
    Error (Compile_error.parse_errors position [
      "parameter lists must be pairs of a name and a type";
      "\n\tplease use the correct form [name1 type1 name2 type2]"
    ])

let parse_function_header position header_forms =
  match List.rev header_forms with
  | return_type :: { Source_form.position; value = Source_form.Vector raw_parameters; _ } :: [] ->
    let* parsed_parameters = parse_function_parameters position raw_parameters in
    let* return_type = parse_type_expr return_type in
    return (parsed_parameters, return_type)
  | _ -> begin
    Error (Compile_error.parse_errors position [
      "function headers must be a vector containing a vector of parameters and the return type";
      "\n\tplease use the correct form (fn [[name type] type] <body-expression>)"
    ])
  end

let parse_function recursively_parse position = function
  | { Source_form.position = header_position; value = Source_form.Vector raw_header; _ } :: body :: [] ->
    let* (parameters, return_type) = parse_function_header header_position raw_header in
    let* body_form = recursively_parse body in
    return { Form.position; parsed = Form.Fn { parameters; return_type; body_form } }
  | _ -> begin
    Error (Compile_error.parse_errors position [
      "function forms must contain a function header and singular body expression";
      "\n\tplease use the correct form (fn [[name type] type] <body-expression>)"
    ])
  end

let parse_if recursively_parse position = function
  | test_form :: if_form :: else_form :: [] ->
      let* test_form = recursively_parse test_form in
      let* if_form = recursively_parse if_form in
      let* else_form = recursively_parse else_form in
      return { Form.position; parsed = Form.If { test_form; if_form; else_form } }
  | _ -> begin
    Error (Compile_error.parse_errors position [
      "if forms must contain a test expression, an if expression and an else expression";
      "\n\tplease use the correct form (if <test-expression> <then-expression> <else-expression>)"
    ])
  end

let parse_binding recursively_parse = function
  | ({ Source_form.value = Source_form.Symbol name; _ }, expression) ->
    let* expression = recursively_parse expression in
    return (Form.Binding.from_form name expression)
  | (first_form, _) -> begin
    Error (Compile_error.parse_errors first_form.position [
      "a binding must be a pair of a name and an expression";
      "\n\tplease use the correct form [name1 expression1 ... nameN expressionN]"
    ])
  end

let parse_bindings recursively_parse position bindings =
  let parse_binding binding parsed_bindings =
    let* parsed_bindings = parsed_bindings in
    let* parsed_binding = parse_binding recursively_parse binding in
    return (parsed_binding :: parsed_bindings)
  in
  if (List.length bindings mod 2) = 0 then
    List.fold_right parse_binding (List.as_pairs bindings) (Ok [])
  else
    Error (Compile_error.parse_errors position [
      "bindings must be pairs of a name and an expression";
      "\n\tplease use the correct form ([name1 expression1 ... nameN expressionN])"
    ])

let parse_let recursively_parse position = function
  | { Source_form.position = binding_position; value = Source_form.Vector bindings; _ } :: body :: [] ->
    let* bindings = parse_bindings recursively_parse binding_position bindings in
    let* body_form = recursively_parse body in
    return { Form.position; parsed = Form.Let { bindings; body_form } }
  | _ -> begin
    Error (Compile_error.parse_errors position [
      "let forms must contain a vector of variable bindings and a singular body expression";
      "\n\tplease use the correct form (let [name expression] <body-expression>)"
    ])
  end

let parse_cast recursively_parse position = function
  | target :: body :: [] -> begin
    let* target_type = parse_type_expr target in
    let* body_form = recursively_parse body in
    return { Form.position; parsed = Form.Cast { target_type; body_form } }
  end
  | _ -> begin
    Error (Compile_error.parse_errors position [
      "cast forms must contain a type definition and a singular expression";
      "\n\tplease use the correct form (cast type expression)"
    ])
  end

let parse_apply_arguments recursively_parse arguments =
  let parse_argument argument parsed_arguments =
    let* parsed_arguments = parsed_arguments in
    let* parsed_argument = recursively_parse argument in
    return (parsed_argument :: parsed_arguments) in
  List.fold_right parse_argument arguments (Ok [])

let parse_number_apply recursively_parse apply_position number_position value arguments =
  let* arguments = parse_apply_arguments recursively_parse arguments in
  let callable_form = { Form.position = number_position; parsed = Form.Number value } in
  return { Form.position = apply_position; parsed = Form.Apply { callable_form; arguments } }

let parse_string_apply recursively_parse apply_position string_position value arguments =
  let* arguments = parse_apply_arguments recursively_parse arguments in
  let callable_form = { Form.position = string_position; parsed = Form.String value } in
  return { Form.position = apply_position; parsed = Form.Apply { callable_form; arguments } }

let parse_symbol_apply recursively_parse apply_position symbol_position function_name arguments =
  let* function_name = parse_name_expr symbol_position function_name in
  let* arguments = parse_apply_arguments recursively_parse arguments in
  let callable_form = { Form.position = symbol_position; parsed = Form.Symbol function_name } in
  return { Form.position = apply_position; parsed = Form.Apply { callable_form; arguments } }

let parse_function_apply recursively_parse apply_position callable_position function_value arguments =
  let form_to_parse = { Source_form.position = callable_position; value = Source_form.List function_value } in
  let* callable_form = recursively_parse form_to_parse in
  let* arguments = parse_apply_arguments recursively_parse arguments in
  return { Form.position = apply_position; parsed = Form.Apply { callable_form; arguments } }

let parse_builtin recursively_parse list_position builtin_position builtin (arguments: Source_form.t list) =
  if builtin = "get" then parse_get list_position arguments
  else if builtin = "set!" then parse_set recursively_parse list_position arguments
  else if builtin = "fn" then parse_function recursively_parse list_position arguments
  else if builtin = "if" then parse_if recursively_parse list_position arguments
  else if builtin = "let" then parse_let recursively_parse list_position arguments
  else if builtin = "cast" then parse_cast recursively_parse list_position arguments
  else if builtin = "def" then parse_def recursively_parse list_position arguments
  else parse_symbol_apply recursively_parse list_position builtin_position builtin arguments

let parse_symbol position symbol =
  let* symbol = parse_name_expr position symbol in
  return { Form.position; parsed = Form.Symbol symbol }

let parse_list recursively_parse list_position = function
  | { Source_form.position = number_position; Source_form.value = Source_form.Number value; _ } :: arguments ->
      parse_number_apply recursively_parse list_position number_position value arguments
  | { Source_form.position = string_position; Source_form.value = Source_form.String value; _ } :: arguments ->
      parse_string_apply recursively_parse list_position string_position value arguments
  | { Source_form.position = builtin_position; Source_form.value = Source_form.Symbol operation; _ } :: arguments ->
      parse_builtin recursively_parse list_position builtin_position operation arguments
  | { Source_form.position = callable_position; Source_form.value = Source_form.List expression; _ } :: arguments ->
      parse_function_apply recursively_parse list_position callable_position expression arguments
  | _ -> begin
    Error (Compile_error.parse_errors list_position [
      sprintf "failed to parse invalid list form";
      "\n\tplease check your syntax"
    ])
  end

let rec parse_form { Source_form.position; value; _ } =
  match value with
  | Source_form.Number value ->
      Ok { Form.position; parsed = Form.Number value }
  | Source_form.String value ->
      Ok { Form.position; parsed = Form.String value }
  | Source_form.Symbol value -> parse_symbol position value
  | Source_form.List value -> parse_list parse_form position value
  | Source_form.Vector _ -> begin
    Error (Compile_error.parse_errors position [
      "found unexpected vector form";
      "\n\tplease check your syntax"
    ])
  end
  (*| Source_form.Extension value -> parse_extension parse_form position value*)
