open Printf
open Common.Extensions
open Common.Extensions.Result
open Common.Extensions.Result.Syntax

module Form = Ast.Semantic_form

module Result = struct
  type t = (Form.t, Compile_error.t) result

  let inspect result =
    Result.inspect Form.inspect Compile_error.to_string result
end

let metadata_from_position position =
  let { Stream_position.line_number; char_number; _ } = position in
  { Metadata.line_num = line_number; char_num = char_number; source = "<SOURCE HERE>" }

let parse_qualified_name position name =
  match String.split_on_char '/' name with
  | module_name :: object_name :: [] -> begin
    let module_segments = String.split_on_char '.' module_name in
    let module_segments = List.map (Module_name.Segment.from_string) module_segments in
    match List.rev module_segments with
    | basename :: segments -> begin
      let module_path = Module_name.Path.from_segments segments in
      let module_name = Module_name.from_path_and_base module_path basename in
      Ok (Name_expr.QualName (module_name, object_name))
    end
    | _ ->
        Error (Compile_error.parse_errors position [
          "qualified names must contain a full path to a module, ";
          sprintf "but instead found: %s" name;
          "\n\tplease use the correct form foo/bar.baz instead of foo/baz"
      ])
  end
  | _ ->
      Error (Compile_error.parse_errors position [
        "qualified names must contain both module tree and path information, ";
        sprintf "but instead found: %s" name;
        "\n\tplease use the correct form foo/bar.baz instead of bar.baz"
    ])

let parse_name_expr position name =
  if String.contains name '.' then parse_qualified_name position name
  else Ok (Name_expr.BareName name)

let parse_type_list parse_type_expr' forms_to_parse =
  List.fold_right (fun type_form parsed_types ->
    let* parsed_types = parsed_types in
    let* next_type = parse_type_expr' type_form in
    return (next_type :: parsed_types)) forms_to_parse (Ok [])

let invalid_type_error position =
  Error (Compile_error.parse_errors position [
    "type expressions must be of a valid form, ";
    sprintf "but instead found: %s" "<SOURCE HERE>";
    "\n\tplease use a correct simple or aggregate type"
  ])

let rec parse_type_expr { Source_form.position; value } =
  match value with
  | Symbol value -> begin
    let* parsed_type = parse_name_expr position value in
    return (Type_expr.SimpleType parsed_type)
  end
  | Vector forms -> begin
   match parse_type_list parse_type_expr forms with
   | Error e -> Error e
   | Ok [] ->
       Error (Compile_error.parse_errors position [
        "aggregate type expressions must contain 1 or more types, instead found an empty expression";
        "\n\tplease use the correct form [type1 type2 type3]"
     ])
   | Ok parsed_types -> Ok (Type_expr.CompoundType parsed_types)
  end
  | _ -> invalid_type_error position

let parse_var_def = function
  | { Source_form.value = Source_form.Symbol raw_name; _ } :: type_form :: [] -> begin
    let* parsed_type = parse_type_expr type_form in
    let parsed_name = Identifier.from_string raw_name in
    return (parsed_name, parsed_type)
  end
  | first_form :: last_form :: [] -> begin
    Error (Compile_error.parse_errors first_form.position [
      "variable definitions must be pairs of a name and a type, ";
      sprintf "but instead found: [%s %s]" first_form.position.source last_form.position.source;
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
      let name = Identifier.from_string name in
      let metadata = metadata_from_position position in
      return { Form.metadata; parsed = Form.Def { name; body_form } }
  | { Source_form.value = Source_form.Symbol _; _ } :: invalid_form :: [] -> begin
    let invalid_form = sprintf "%s" invalid_form.position.source in
    Error (Compile_error.parse_errors position [
      "top-level definitions must evaluate to a constant value, ";
      sprintf "but instead found: %s" invalid_form;
      "\n\tplease use the correct form (def name <constant>)"
    ])
  end
  | _ -> begin
    Error (Compile_error.parse_errors position [
      "top-level variable definitions must provide a name and an expr, ";
      sprintf "but instead found: (def %s)" "<SOURCE HERE>";
      "\n\tplease use the correct form (def name <constant>)"
    ])
  end

let parse_get position = function
  | [{ Source_form.position; value = Source_form.Symbol target; _ }; { value = Source_form.Symbol field; _ }] ->
    let* target = parse_name_expr position target in
    let metadata = metadata_from_position position in
    let target_form = { Form.metadata; parsed = Form.Symbol target } in
    let field = Identifier.from_string field in
    return { Form.metadata; parsed = Form.Get { target_form; field } }
  | _ -> begin
    Error (Compile_error.parse_errors position [
      "get expression must provide the record and field name, ";
      sprintf "but instead found: (get %s)" "<SOURCE HERE>";
      "\n\tplease use the correct form (get record field)"
    ])
  end

let parse_set recursively_parse position = function
  | [{ Source_form.position; value = Source_form.Symbol target; _ }; { value = Source_form.Symbol field; _ }; body] ->
    let* target = parse_name_expr position target in
    let field = Identifier.from_string field in
    let* body_form = recursively_parse body in
    let metadata = metadata_from_position position in
    let target_form = { Form.metadata; parsed = Form.Symbol target } in
    return { Form.metadata; parsed = Form.Set { target_form; field; body_form } }
  | _ -> begin
    Error (Compile_error.parse_errors position [
      "set expression must provide record and field names, and expression, ";
      sprintf "but instead found: (set! %s)" "<SOURCE HERE>";
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
      "parameter lists must be pairs of a name and a type, ";
      sprintf "but instead found an odd number of forms: [%s]" "<SOURCE HERE>";
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
      "function headers must be a vector containing a vector of parameters and the return type, ";
      sprintf "but instead found: [%s]" "<SOURCE HERE>";
      "\n\tplease use the correct form (fn [[name type] type] <body>)"
    ])
  end

let parse_function recursively_parse position = function
  | { Source_form.position = header_position; value = Source_form.Vector raw_header; _ } :: body :: [] ->
    let* (parameters, return_type) = parse_function_header header_position raw_header in
    let* body_form = recursively_parse body in
    let metadata = metadata_from_position position in
    return { Form.metadata; parsed = Form.Fn { parameters; return_type; body_form } }
  | _ -> begin
    Error (Compile_error.parse_errors position [
      "function forms must contain a function header and singular body expression, ";
      sprintf "but instead found: (fn %s)" "<SOURCE HERE>";
      "\n\tplease use the correct form (fn [[name type] type] <body>)"
    ])
  end

let parse_if recursively_parse position = function
  | test_form :: if_form :: else_form :: [] ->
      let* test_form = recursively_parse test_form in
      let* if_form = recursively_parse if_form in
      let* else_form = recursively_parse else_form in
      let metadata = metadata_from_position position in
      return { Form.metadata; parsed = Form.If { test_form; if_form; else_form } }
  | _ -> begin
    Error (Compile_error.parse_errors position [
      "if forms must contain a test expression, an if expression and an else expression, ";
      sprintf "but instead found: (if %s)" "<SOURCE HERE>";
      "\n\tplease use the correct form (if <test> <then> <else>)"
    ])
  end

let parse_binding recursively_parse = function
  | ({ Source_form.value = Source_form.Symbol name; _ }, expression) ->
    let name = Identifier.from_string name in
    let* expression = recursively_parse expression in
    return (Form.Binding.from_form name expression)
  | (first_form, _) -> begin
    Error (Compile_error.parse_errors first_form.position [
      "a binding must be a pair of a name and an expression, ";
      sprintf "but instead found: [%s %s]" "<SOURCE HERE>" "<SOURCE HERE>";
      "\n\tplease use the correct form [name1 expr1 ... nameN exprN]"
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
      "bindings must be pairs of a name and an expression, ";
      sprintf "but instead found an odd number of forms: [%s]" "<SOURCE HERE>";
      "\n\tplease use the correct form ([name1 expr1 ... nameN exprN])"
    ])

let parse_let recursively_parse position = function
  | { Source_form.position = binding_position; value = Source_form.Vector bindings; _ } :: body :: [] ->
    let* bindings = parse_bindings recursively_parse binding_position bindings in
    let* body_form = recursively_parse body in
    let metadata = metadata_from_position position in
    return { Form.metadata; parsed = Form.Let { bindings; body_form } }
  | _ -> begin
    Error (Compile_error.parse_errors position [
      "let forms must contain a vector of variable bindings and a singular body expression, ";
      sprintf "but instead found: (let %s)" "<SOURCE HERE>";
      "\n\tplease use the correct form (let [name expr] <body>)"
    ])
  end

let parse_cast recursively_parse position = function
  | target :: body :: [] -> begin
    let* target_type = parse_type_expr target in
    let* body_form = recursively_parse body in
    let metadata = metadata_from_position position in
    return { Form.metadata; parsed = Form.Cast { target_type; body_form } }
  end
  | _ -> begin
    Error (Compile_error.parse_errors position [
      "cast forms must contain a type definition and a singular expression, ";
      sprintf "but instead found: (cast %s)" "<SOURCE HERE>";
      "\n\tplease use the correct form (cast type expr)"
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
  let number_metadata = metadata_from_position number_position in
  let callable_form = { Form.metadata = number_metadata; parsed = Form.NumLit value } in
  let apply_metadata = metadata_from_position apply_position in
  return { Form.metadata = apply_metadata; parsed = Form.Apply { callable_form; arguments } }

let parse_string_apply recursively_parse apply_position string_position value arguments =
  let* arguments = parse_apply_arguments recursively_parse arguments in
  let string_metadata = metadata_from_position string_position in
  let callable_form = { Form.metadata = string_metadata; parsed = Form.StrLit value } in
  let apply_metadata = metadata_from_position apply_position in
  return { Form.metadata = apply_metadata; parsed = Form.Apply { callable_form; arguments } }

let parse_symbol_apply recursively_parse apply_position symbol_position function_name arguments =
  let* function_name = parse_name_expr symbol_position function_name in
  let* arguments = parse_apply_arguments recursively_parse arguments in
  let symbol_metadata = metadata_from_position symbol_position in
  let callable_form = { Form.metadata = symbol_metadata; parsed = Form.Symbol function_name } in
  let apply_metadata = metadata_from_position apply_position in
  return { Form.metadata = apply_metadata; parsed = Form.Apply { callable_form; arguments } }

let parse_function_apply recursively_parse apply_position callable_position function_value arguments =
  let form_to_parse = { Source_form.position = callable_position; value = Source_form.List function_value } in
  let* callable_form = recursively_parse form_to_parse in
  let* arguments = parse_apply_arguments recursively_parse arguments in
  let apply_metadata = metadata_from_position apply_position in
  return { Form.metadata = apply_metadata; parsed = Form.Apply { callable_form; arguments } }

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
  let metadata = metadata_from_position position in
  return { Form.metadata; parsed = Form.Symbol symbol }

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
      sprintf "failed to parse invalid form: %s" "<SOURCE HERE>";
      "\n\tplease check your syntax"
    ])
  end

let rec parse_form { Source_form.position; value; _ } =
  match value with
  | Source_form.Number value ->
      let metadata = metadata_from_position position in
      Ok { Form.metadata; parsed = Form.NumLit value }
  | Source_form.String value ->
      let metadata = metadata_from_position position in
      Ok { Form.metadata; parsed = Form.StrLit value }
  | Source_form.Symbol value -> parse_symbol position value
  | Source_form.List value -> parse_list parse_form position value
  | Source_form.Vector _ -> begin
    Error (Compile_error.parse_errors position [
      "found unexpected vector form";
      "\n\tplease check your syntax"
    ])
  end
  (*| Source_form.Extension value -> parse_extension parse_form metadata value*)

let toplevel_error position raw =
  Error (Compile_error.parse_errors position [
    sprintf "top-level forms must be either a def or defrecord, instead found %s" raw;
    "\n\tplease use the correct forms at the top-level"
  ])

let parse_form ?check_toplevel:(check_toplevel=true) source_form =
  match source_form with
  | _ when not check_toplevel -> parse_form source_form
  | { Source_form.value = Source_form.List value; _ } -> begin
    match value with
    | { Source_form.position = symbol_position; value = Source_form.Symbol operation; _ } :: args -> begin
      if operation = "def" then parse_def parse_form source_form.position args
      else toplevel_error symbol_position operation
    end
    | _ -> toplevel_error source_form.position "<SOURCE HERE>"
  end
  | { Source_form.position; _ } -> toplevel_error position "<SOURCE HERE>"
