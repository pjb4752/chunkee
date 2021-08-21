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

let create_parse_error position message_fragments =
  Error (Compile_error.create_parse_error position message_fragments)

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
    | _ -> create_parse_error position [
          "qualified names must contain a full path to a module";
          "\n\tplease use the correct form foo/bar.baz instead of foo/baz"
      ]
  end
  | _ -> create_parse_error position [
        "qualified names must contain both module tree and path information";
        "\n\tplease use the correct form foo/bar.baz instead of bar.baz"
    ]

let parse_name position name =
  if String.contains name '.' then parse_qualified_name position name
  else Ok (Unresolved_name.UnqualifiedName name)

let parse_compound_type parse_type' forms_to_parse =
  List.bind_right parse_type' forms_to_parse

let invalid_type_error position =
  create_parse_error position [
    "expected a valid type expression, but did not find one";
    "\n\tplease use a correct simple or aggregate type."
  ]

let rec parse_type { Source_form.position; value } =
  match value with
  | Symbol value -> begin
    let* parsed_type = parse_name position value in
    return (Type_expression.SimpleType parsed_type)
  end
  | Vector forms -> begin
   match parse_compound_type parse_type forms with
   | Error e -> Error e
   | Ok [] -> create_parse_error position [
        "aggregate type expressions must contain 1 or more types, instead found an empty expression";
        "\n\tplease use the correct form [type1 type2 type3]"
     ]
   | Ok parsed_types -> Ok (Type_expression.CompoundType parsed_types)
  end
  | _ -> invalid_type_error position

let parse_parameter = function
  | { Source_form.value = Symbol name; _ } :: type_form :: [] -> begin
    let* parsed_type = parse_type type_form in
    return (name, parsed_type)
  end
  | first_form :: _ -> create_parse_error first_form.position [
      "parameter definitions must be pairs of a name and a type";
      "\n\tplease use the correct form [name type]"
    ]
  | _ -> assert false

let is_const_literal { Source_form.value; _ } =
  match value with
  | Source_form.Number _ | Source_form.String _ -> true
  | Source_form.List ({ value = Source_form.Symbol "fn"; _ } :: _) -> true
  | _ -> false

let parse_def recursively_parse position = function
  | { Source_form.value = Symbol name; _ } :: body_expr :: [] when is_const_literal body_expr ->
      let* body_form = recursively_parse body_expr in
      return (Form.create_def position name body_form)
  | { Source_form.value = Symbol _; _ } :: _ :: [] -> create_parse_error position [
      "top-level definitions must evaluate to a constant value";
      "\n\tplease use the correct form (def name <constant-expression>)"
    ]
  | _ -> create_parse_error position [
      "top-level variable definitions must provide a name and an expr";
      "\n\tplease use the correct form (def name <constant-expression>)"
    ]

let parse_get position = function
  | [{ Source_form.position; value = Symbol target; _ }; { value = Symbol field; _ }] ->
    let* target = parse_name position target in
    let target_form = Form.create_symbol position target in
    return (Form.create_get position target_form field)
  | _ -> create_parse_error position [
      "get expression must provide the record and field name";
      "\n\tplease use the correct form (get record field)"
    ]

let parse_set recursively_parse position = function
  | [{ Source_form.position; value = Symbol target; _ }; { value = Symbol field; _ }; body] ->
    let* target = parse_name position target in
    let* body_form = recursively_parse body in
    let target_form = Form.create_symbol position target in
    return (Form.create_set position target_form field body_form)
  | _ -> create_parse_error position [
      "set expression must provide record and field names, and an expression";
      "\n\tplease use the correct form (set! record field expression)"
    ]

let parse_function_parameters position parameters =
  if (List.length parameters mod 2) = 0 then
    let parse_parameter (name, param_type) =
      let* (name, param_type) = parse_parameter [name; param_type] in
      return (Form.Parameter.create name param_type )
    in
    List.bind_right parse_parameter @@ List.as_pairs parameters
  else create_parse_error position [
      "parameter lists must be pairs of a name and a type";
      "\n\tplease use the correct form [name1 type1 name2 type2]"
    ]

let parse_function_header position header_forms =
  match List.rev header_forms with
  | return_type :: { Source_form.position; value = Vector raw_parameters; _ } :: [] ->
    let* parsed_parameters = parse_function_parameters position raw_parameters in
    let* return_type = parse_type return_type in
    return (parsed_parameters, return_type)
  | _ -> create_parse_error position [
      "function headers must be a vector containing a vector of parameters and the return type";
      "\n\tplease use the correct form (fn [[name type] type] <body-expression>)"
    ]

let parse_function recursively_parse position = function
  | { Source_form.position = header_position; value = Vector raw_header; _ } :: body :: [] ->
    let* (parameters, return_type) = parse_function_header header_position raw_header in
    let* body_form = recursively_parse body in
    return (Form.create_fn position parameters return_type body_form)
  | _ -> create_parse_error position [
      "function forms must contain a function header and singular body expression";
      "\n\tplease use the correct form (fn [[name type] type] <body-expression>)"
    ]

let parse_if recursively_parse position = function
  | test_form :: if_form :: else_form :: [] ->
      let* test_form = recursively_parse test_form in
      let* if_form = recursively_parse if_form in
      let* else_form = recursively_parse else_form in
      return (Form.create_if position test_form if_form else_form)
  | _ -> create_parse_error position [
      "if forms must contain a test expression, an if expression and an else expression";
      "\n\tplease use the correct form (if <test-expression> <then-expression> <else-expression>)"
    ]

let parse_binding recursively_parse = function
  | ({ Source_form.value = Symbol name; _ }, expression) ->
    let* expression = recursively_parse expression in
    return (Form.Binding.create name expression)
  | (first_form, _) -> create_parse_error first_form.position [
      "a binding must be a pair of a name and an expression";
      "\n\tplease use the correct form [name1 expression1 ... nameN expressionN]"
    ]

let parse_bindings recursively_parse position bindings =
  if (List.length bindings mod 2) = 0 then
    List.bind_right (fun binding -> parse_binding recursively_parse binding) @@ List.as_pairs bindings
  else create_parse_error position [
      "bindings must be pairs of a name and an expression";
      "\n\tplease use the correct form ([name1 expression1 ... nameN expressionN])"
    ]

let parse_let recursively_parse position = function
  | { Source_form.position = binding_position; value = Vector bindings; _ } :: body :: [] ->
    let* bindings = parse_bindings recursively_parse binding_position bindings in
    let* body_form = recursively_parse body in
    return (Form.create_let position bindings body_form)
  | _ -> create_parse_error position [
      "let forms must contain a vector of variable bindings and a singular body expression";
      "\n\tplease use the correct form (let [name expression] <body-expression>)"
    ]

let parse_cast recursively_parse position = function
  | target :: body :: [] -> begin
    let* target_type = parse_type target in
    let* body_form = recursively_parse body in
    return (Form.create_cast position target_type body_form)
  end
  | _ -> create_parse_error position [
      "cast forms must contain a type definition and a singular expression";
      "\n\tplease use the correct form (cast type expression)"
    ]

let parse_apply_arguments recursively_parse arguments =
  List.bind_right recursively_parse arguments

let parse_number_apply recursively_parse apply_position number_position value arguments =
  let* arguments = parse_apply_arguments recursively_parse arguments in
  let callable_form = Form.create_number number_position value in
  return (Form.create_apply apply_position callable_form arguments)

let parse_string_apply recursively_parse apply_position string_position value arguments =
  let* arguments = parse_apply_arguments recursively_parse arguments in
  let callable_form = Form.create_string string_position value in
  return (Form.create_apply apply_position callable_form arguments)

let parse_symbol_apply recursively_parse apply_position symbol_position function_name arguments =
  let* function_name = parse_name symbol_position function_name in
  let* arguments = parse_apply_arguments recursively_parse arguments in
  let callable_form = Form.create_symbol symbol_position function_name in
  return (Form.create_apply apply_position callable_form arguments)

let parse_function_apply recursively_parse apply_position callable_position function_value arguments =
  let form_to_parse = { Source_form.position = callable_position; value = List function_value } in
  let* callable_form = recursively_parse form_to_parse in
  let* arguments = parse_apply_arguments recursively_parse arguments in
  return (Form.create_apply apply_position callable_form arguments)

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
  let* symbol = parse_name position symbol in
  return (Form.create_symbol position symbol)

let parse_list recursively_parse list_position = function
  | { Source_form.position = number_position; value = Number value; _ } :: arguments ->
      parse_number_apply recursively_parse list_position number_position value arguments
  | { Source_form.position = string_position; value = String value; _ } :: arguments ->
      parse_string_apply recursively_parse list_position string_position value arguments
  | { Source_form.position = builtin_position; value = Symbol operation; _ } :: arguments ->
      parse_builtin recursively_parse list_position builtin_position operation arguments
  | { Source_form.position = callable_position; value = List expression; _ } :: arguments ->
      parse_function_apply recursively_parse list_position callable_position expression arguments
  | _ -> create_parse_error list_position [
      sprintf "failed to parse invalid list form";
      "\n\tplease check your syntax"
    ]

let rec parse_form { Source_form.position; value; _ } =
  match value with
  | Source_form.Number value -> Ok (Form.create_number position value)
  | Source_form.String value -> Ok (Form.create_string position value)
  | Source_form.Symbol value -> parse_symbol position value
  | Source_form.List elements -> parse_list parse_form position elements
  | Source_form.Vector _ -> create_parse_error position ["I don't understand vectors yet"]
  (*| Source_form.Extension value -> parse_extension parse_form position value*)
