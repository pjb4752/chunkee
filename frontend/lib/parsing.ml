open Lexing
open Printf
open Common.Extensions
open Common.Extensions.Result
open Common.Extensions.Result.Syntax

module Node = Ast.Parsed_node

module Result = struct
  type t = (Node.t, Cmpl_err.t) result

  let inspect result =
    Result.inspect result Node.inspect Cmpl_err.to_string
end

let build_error_prefix { Metadata.line_num; char_num } =
  sprintf "in expression at %d:%d" line_num char_num

let parse_qualified_name metadata name =
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
        let prefix = build_error_prefix metadata in
        Error (Cmpl_err.parse_errors metadata prefix [
          "qualified names must contain a full path to a module, ";
          sprintf "but instead found: %s" name;
          "\n\tplease use the correct form foo/bar.baz instead of foo/baz"
      ])
  end
  | _ ->
      let prefix = build_error_prefix metadata in
      Error (Cmpl_err.parse_errors metadata prefix [
        "qualified names must contain both module tree and path information, ";
        sprintf "but instead found: %s" name;
        "\n\tplease use the correct form foo/bar.baz instead of bar.baz"
    ])

let parse_name_expr metadata name =
  if String.contains name '.' then parse_qualified_name metadata name
  else Ok (Name_expr.BareName name)

let parse_type_list parse_type_expr' forms_to_parse =
  List.fold_right (fun type_form parsed_types ->
    let* parsed_types = parsed_types in
    let* next_type = parse_type_expr' type_form in
    return (next_type :: parsed_types)) forms_to_parse (Ok [])

let invalid_type_error metadata raw_expression =
  let prefix = build_error_prefix metadata in
  Error (Cmpl_err.parse_errors metadata prefix [
    "type expressions must be of a valid form, ";
    sprintf "but instead found: %s" raw_expression;
    "\n\tplease use a correct simple or aggregate type"
  ])

let rec parse_type_expr { Form.metadata; source; lexed } =
  match lexed with
  | Form.Symbol value -> begin
    let* parsed_type = parse_name_expr metadata value in
    return (Type_expr.SimpleType parsed_type)
  end
  | Form.Vector forms -> begin
   match parse_type_list parse_type_expr forms with
   | Error e -> Error e
   | Ok [] ->
       let prefix = build_error_prefix metadata in
       Error (Cmpl_err.parse_errors metadata prefix [
        "aggregate type expressions must contain 1 or more types, instead found an empty expression";
        "\n\tplease use the correct form [type1 type2 type3]"
     ])
   | Ok parsed_types -> Ok (Type_expr.CompoundType parsed_types)
  end
  | _ -> invalid_type_error metadata source

let parse_var_def = function
  | { Form.lexed = Form.Symbol raw_name; _ } :: type_form :: [] -> begin
    let* parsed_type = parse_type_expr type_form in
    let parsed_name = Identifier.from_string raw_name in
    return (parsed_name, parsed_type)
  end
  | first_form :: last_form :: [] -> begin
    let prefix = build_error_prefix first_form.metadata in
    Error (Cmpl_err.parse_errors first_form.metadata prefix [
      "variable definitions must be pairs of a name and a type, ";
      sprintf "but instead found: [%s %s]" first_form.source last_form.source;
      "\n\tplease use the correct form [name type]"
    ])
  end
  | _ -> assert false

let is_const_literal { Form.lexed; _ } =
  match lexed with
  | Form.Number _ | Form.String _ -> true
  | Form.List ({ lexed = Form.Symbol "fn"; _ } :: _) -> true
  | _ -> false

let parse_def recursively_parse metadata = function
  | { Form.lexed = Form.Symbol name; _ } :: expression :: [] when is_const_literal expression ->
      let* expression = recursively_parse expression in
      let name = Identifier.from_string name in
      return (Node.Def (name, expression, metadata))
  | { Form.lexed = Form.Symbol _; _ } :: invalid_form :: [] -> begin
    let prefix = build_error_prefix metadata in
    let invalid_form = sprintf "%s" invalid_form.source in
    Error (Cmpl_err.parse_errors metadata prefix [
      "top-level definitions must evaluate to a constant value, ";
      sprintf "but instead found: %s" invalid_form;
      "\n\tplease use the correct form (def name <constant>)"
    ])
  end
  | invalid_forms -> begin
    let prefix = build_error_prefix metadata in
    let invalid_forms = String.concat " " @@ List.map Form.source invalid_forms in
    Error (Cmpl_err.parse_errors metadata prefix [
      "top-level variable definitions must provide a name and an expr, ";
      sprintf "but instead found: (def %s)" invalid_forms;
      "\n\tplease use the correct form (def name <constant>)"
    ])
  end

let parse_get metadata = function
  | [{ Form.metadata; lexed = Form.Symbol record; _ }; { lexed = Form.Symbol field; _ }] ->
    let* record_name = parse_name_expr metadata record in
    let record_name = Node.Symbol (record_name, metadata) in
    let field_name = Identifier.from_string field in
    return (Node.Get (record_name, field_name, metadata))
  | invalid_forms -> begin
    let prefix = build_error_prefix metadata in
    let invalid_forms = String.concat " " @@ List.map Form.source invalid_forms in
    Error (Cmpl_err.parse_errors metadata prefix [
      "get expression must provide the record and field name, ";
      sprintf "but instead found: (get %s)" invalid_forms;
      "\n\tplease use the correct form (get record field)"
    ])
  end

let parse_set recursively_parse metadata = function
  | [{ Form.metadata; lexed = Form.Symbol record; _ }; { lexed = Form.Symbol field; _ }; expression] ->
    let* record_name = parse_name_expr metadata record in
    let field_name = Identifier.from_string field in
    let* expression = recursively_parse expression in
    let record_name = Node.Symbol (record_name, metadata) in
    return (Node.Set (record_name, field_name, expression, metadata))
  | invalid_forms -> begin
    let prefix = build_error_prefix metadata in
    let invalid_forms = String.concat " " @@ List.map Form.source invalid_forms in
    Error (Cmpl_err.parse_errors metadata prefix [
      "set expression must provide record and field names, and expression, ";
      sprintf "but instead found: (set! %s)" invalid_forms;
      "\n\tplease use the correct form (set! record field expression)"
    ])
  end

let parse_function_parameters metadata parameters =
  let parse_parameter (name, param_type) parsed_params =
    let* parsed_params = parsed_params in
    let* (name, param_type) = parse_var_def [name; param_type] in
    let param = Node.VarDef.from_parts name param_type in
    return (param :: parsed_params) in
  if (List.length parameters mod 2) = 0 then
    List.fold_right parse_parameter (List.as_pairs parameters) (Ok [])
  else
    let prefix = build_error_prefix metadata in
    let raw_form = String.concat " " @@ List.map Form.source parameters in
    Error (Cmpl_err.parse_errors metadata prefix [
      "parameter lists must be pairs of a name and a type, ";
      sprintf "but instead found an odd number of forms: [%s]" raw_form;
      "\n\tplease use the correct form [name1 type1 name2 type2]"
    ])

let parse_function_header metadata header_forms =
  match List.rev header_forms with
  | return_type :: { Form.metadata; lexed = Form.Vector raw_parameters; _ } :: [] ->
    let* parsed_parameters = parse_function_parameters metadata raw_parameters in
    let* return_type = parse_type_expr return_type in
    return (parsed_parameters, return_type)
  | invalid_forms -> begin
    let prefix = build_error_prefix metadata in
    let invalid_header = String.concat " " @@ List.map Form.source invalid_forms in
    Error (Cmpl_err.parse_errors metadata prefix [
      "function headers must be a vector containing a vector of parameters and the return type, ";
      sprintf "but instead found: [%s]" invalid_header;
      "\n\tplease use the correct form (fn [[name type] type] <body>)"
    ])
  end

let parse_function recursively_parse metadata = function
  | { Form.metadata = header_metadata; lexed = Form.Vector raw_header; _ } :: raw_body :: [] ->
    let* (parameters, return_type) = parse_function_header header_metadata raw_header in
    let* body = recursively_parse raw_body in
    return (Node.Fn (parameters, return_type, body, metadata))
  | invalid_forms -> begin
    let prefix = build_error_prefix metadata in
    let invalid_forms = String.concat " " @@ List.map Form.source invalid_forms in
    Error (Cmpl_err.parse_errors metadata prefix [
      "function forms must contain a function header and singular body expression, ";
      sprintf "but instead found: (fn %s)" invalid_forms;
      "\n\tplease use the correct form (fn [[name type] type] <body>)"
    ])
  end

let parse_if recursively_parse metadata = function
  | raw_test :: raw_if :: raw_else :: [] ->
      let* parsed_test = recursively_parse raw_test in
      let* parsed_if = recursively_parse raw_if in
      let* parsed_else = recursively_parse raw_else in
      return (Node.If (parsed_test, parsed_if, parsed_else, metadata))
  | invalid_forms -> begin
    let prefix = build_error_prefix metadata in
    let invalid_forms = String.concat " " @@ List.map Form.source invalid_forms in
    Error (Cmpl_err.parse_errors metadata prefix [
      "if forms must contain a test expression, an if expression and an else expression, ";
      sprintf "but instead found: (if %s)" invalid_forms;
      "\n\tplease use the correct form (if <test> <then> <else>)"
    ])
  end

let parse_binding recursively_parse = function
  | ({ Form.lexed = Form.Symbol name; _ }, expression) ->
    let name = Identifier.from_string name in
    let* expression = recursively_parse expression in
    return (Node.Binding.from_node name expression)
  | (first_form, (second_form: Form.t)) -> begin
    let prefix = build_error_prefix first_form.metadata in
    Error (Cmpl_err.parse_errors first_form.metadata prefix [
      "a binding must be a pair of a name and an expression, ";
      sprintf "but instead found: [%s %s]" first_form.source second_form.source;
      "\n\tplease use the correct form [name1 expr1 ... nameN exprN]"
    ])
  end

let parse_bindings recursively_parse metadata bindings =
  let parse_binding binding parsed_bindings =
    let* parsed_bindings = parsed_bindings in
    let* parsed_binding = parse_binding recursively_parse binding in
    return (parsed_binding :: parsed_bindings)
  in
  if (List.length bindings mod 2) = 0 then
    List.fold_right parse_binding (List.as_pairs bindings) (Ok [])
  else
    let prefix = build_error_prefix metadata in
    let invalid_bindings = String.concat " " @@ List.map Form.source bindings in
    Error (Cmpl_err.parse_errors metadata prefix [
      "bindings must be pairs of a name and an expression, ";
      sprintf "but instead found an odd number of forms: [%s]" invalid_bindings;
      "\n\tplease use the correct form ([name1 expr1 ... nameN exprN])"
    ])

let parse_let recursively_parse metadata = function
  | { Form.metadata = binding_metadata; lexed = Form.Vector bindings; _ } :: body :: [] ->
    let* bindings = parse_bindings recursively_parse binding_metadata bindings in
    let* body = recursively_parse body in
    return (Node.Let (bindings, body, metadata))
  | invalid_forms -> begin
    let prefix = build_error_prefix metadata in
    let invalid_forms = String.concat " " @@ List.map Form.source invalid_forms in
    Error (Cmpl_err.parse_errors metadata prefix [
      "let forms must contain a vector of variable bindings and a singular body expression, ";
      sprintf "but instead found: (let %s)" invalid_forms;
      "\n\tplease use the correct form (let [name expr] <body>)"
    ])
  end

let parse_cast recursively_parse metadata = function
  | raw_type :: expression :: [] -> begin
    let* parsed_type = parse_type_expr raw_type in
    let* expression = recursively_parse expression in
    return (Node.Cast (parsed_type, expression, metadata))
  end
  | invalid_forms -> begin
    let prefix = build_error_prefix metadata in
    let invalid_forms = String.concat " " @@ List.map Form.source invalid_forms in
    Error (Cmpl_err.parse_errors metadata prefix [
      "cast forms must contain a type definition and a singular expression, ";
      sprintf "but instead found: (cast %s)" invalid_forms;
      "\n\tplease use the correct form (cast type expr)"
    ])
  end

let parse_apply_arguments recursively_parse arguments =
  let parse_argument argument parsed_arguments =
    let* parsed_arguments = parsed_arguments in
    let* parsed_argument = recursively_parse argument in
    return (parsed_argument :: parsed_arguments) in
  List.fold_right parse_argument arguments (Ok [])

let parse_number_apply recursively_parse metadata number_value arguments =
  let* arguments = parse_apply_arguments recursively_parse arguments in
  let apply_target = Node.NumLit (number_value, metadata) in
  return (Node.Apply (apply_target, arguments, metadata))

let parse_string_apply recursively_parse metadata string_value arguments =
  let* arguments = parse_apply_arguments recursively_parse arguments in
  let apply_target = Node.StrLit (string_value, metadata) in
  return (Node.Apply (apply_target, arguments, metadata))

let parse_symbol_apply recursively_parse metadata function_name arguments =
  let* function_name = parse_name_expr metadata function_name in
  let* arguments = parse_apply_arguments recursively_parse arguments in
  let apply_target = Node.Symbol (function_name, metadata) in
  return (Node.Apply (apply_target, arguments, metadata))

let parse_function_apply recursively_parse metadata source function_value arguments =
  let* parsed_function = recursively_parse { Form.metadata; source; lexed = Form.List function_value } in
  let* arguments = parse_apply_arguments recursively_parse arguments in
  return (Node.Apply (parsed_function, arguments, metadata))

let parse_builtin recursively_parse metadata builtin (arguments: Form.t list) =
  if builtin = "get" then parse_get metadata arguments
  else if builtin = "set!" then parse_set recursively_parse metadata arguments
  else if builtin = "fn" then parse_function recursively_parse metadata arguments
  else if builtin = "if" then parse_if recursively_parse metadata arguments
  else if builtin = "let" then parse_let recursively_parse metadata arguments
  else if builtin = "cast" then parse_cast recursively_parse metadata arguments
  else if builtin = "def" then parse_def recursively_parse metadata arguments
  else parse_symbol_apply recursively_parse metadata builtin arguments

let parse_symbol metadata symbol =
  let* symbol = parse_name_expr metadata symbol in
  return (Node.Symbol (symbol, metadata))

let parse_list recursively_parse metadata = function
  | { Form.metadata; lexed = Form.Number value; _ } :: arguments ->
      parse_number_apply recursively_parse metadata value arguments
  | { Form.metadata; lexed = Form.String value; _ } :: arguments ->
      parse_string_apply recursively_parse metadata value arguments
  | { Form.metadata; lexed = Form.Symbol operation; _ } :: arguments ->
      parse_builtin recursively_parse metadata operation arguments
  | { Form.metadata; source; lexed = Form.List expression } :: arguments ->
      parse_function_apply recursively_parse metadata source expression arguments
  | invalid_forms -> begin
    let prefix = build_error_prefix metadata in
    let invalid_forms = String.concat " " @@ List.map Form.source invalid_forms in
    Error (Cmpl_err.parse_errors metadata prefix [
      sprintf "failed to parse invalid form: %s" invalid_forms;
      "\n\tplease check your syntax"
    ])
  end

let parse_extension _ metadata _ =
  let prefix = build_error_prefix metadata in
  Error (Cmpl_err.parse_errors metadata prefix ["Bad"])

let rec parse_form { Form. metadata; lexed; _ } =
  match lexed with
  | Form.Number value -> Ok (Node.NumLit (value, metadata))
  | Form.String value -> Ok (Node.StrLit (value, metadata))
  | Form.Symbol value -> parse_symbol metadata value
  | Form.List value -> parse_list parse_form metadata value
  | Form.Vector _ -> begin
    let prefix = build_error_prefix metadata in
    Error (Cmpl_err.parse_errors metadata prefix [
      "found unexpected vector form";
      "\n\tplease check your syntax"
    ])
  end
  | Form.Extension value -> parse_extension parse_form metadata value

let toplevel_error metadata raw =
  let prefix = build_error_prefix metadata in
  Error (Cmpl_err.parse_errors metadata prefix [
    sprintf "top-level forms must be either a def or defrecord, instead found %s" raw;
    "\n\tplease use the correct forms at the top-level"
  ])

let parse_node ?check_toplevel:(check_toplevel=true) form =
  match form with
  | _ when not check_toplevel -> parse_form form
  | { Form.lexed = Form.List value; _ } -> begin
    match value with
    | { Form.metadata = symbol_metadata; lexed = Form.Symbol operation; _ } :: args -> begin
      if operation = "def" then parse_def parse_form form.metadata args
      else toplevel_error symbol_metadata operation
    end
    | _ -> toplevel_error form.metadata form.source
  end
  | { Form.metadata; source; _ } -> toplevel_error metadata source
