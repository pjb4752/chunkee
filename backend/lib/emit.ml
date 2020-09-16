open Printf
open Frontend
open Common.Extensions

module Node = Ast.Resolved_node

module Char_map = Map.Make(Char)

let char_maps = Char_map.empty |>
  (Char_map.add '+' (String.to_chars "__plus__")) |>
  (Char_map.add '-' (String.to_chars "__dash__")) |>
  (Char_map.add '*' (String.to_chars "__astx__")) |>
  (Char_map.add '!' (String.to_chars "__exlp__")) |>
  (Char_map.add '?' (String.to_chars "__qstm__")) |>
  (Char_map.add '/' (String.to_chars "__slsh__")) |>
  (Char_map.add '>' (String.to_chars "__grtn__")) |>
  (Char_map.add '<' (String.to_chars "__lstn__")) |>
  (Char_map.add '=' (String.to_chars "__eqls__"))

let is_infix_operator = function
  | Name.Var.Module (module_name, variable_name) -> begin
    let variable_name = Identifier.to_string variable_name in
    match Stdlib.find_lua_module module_name with
    | Some matching_module -> Lua_module.operator_exists matching_module variable_name
    | None -> false
  end
  | Name.Var.Local _ -> false

let emit_module_alias segments = String.concat "_" segments

let escape_char chr =
  Option.value (Char_map.find_opt chr char_maps) ~default:[chr]

let escape_chars chars =
  List.fold_left (fun accumulator chr ->
    escape_char chr |> List.append accumulator) [] chars

let escape_name name =
  String.to_chars name |> escape_chars |> String.from_chars

let emit_require module_name =
  let segments = Module_name.to_segments module_name in
  let segments = List.map Module_name.Segment.to_string segments in
  let module_alias = emit_module_alias segments in
  let require_path = String.concat "." segments in
  sprintf "%s = require 'flopcore.%s'" module_alias require_path

let emit_number value =
  Lua_fragment.make_expression (sprintf "%f" value)

let emit_string value =
  Lua_fragment.make_expression (sprintf "\"%s\"" value)

let find_operator module_name variable_name =
  Option.(
    (Stdlib.find_lua_module module_name) >>= fun matching_module ->
    (Lua_module.find_operator matching_module variable_name) >>= fun operator ->
    return operator)

let emit_name ?wrap_ops:(wrap_ops=true) = function
  | Name.Var.Local local_name -> escape_name local_name
  | Name.Var.Module (module_name, variable_name) ->
    let variable_name = Identifier.to_string variable_name in
    match find_operator module_name variable_name with
    | Some operator when wrap_ops -> Lua_operator.wrapper_function_name operator
    | Some operator -> Lua_operator.name operator
    | None -> escape_name variable_name

let emit_symbol ?wrap_ops:(wrap_ops=true) value =
  Lua_fragment.make_expression @@ emit_name ~wrap_ops:wrap_ops value

let emit_def recursively_emit name_generator name expression =
  let name = Identifier.to_string name |> escape_name in
  let expression_fragment = recursively_emit name_generator expression in
  let result_expression = Lua_fragment.result_expression expression_fragment in
  let assignment = sprintf "%s = %s" name result_expression in
  let def_statement = Lua_fragment.make_unit_statement assignment in
  Lua_fragment.insert_preamble def_statement expression_fragment

let vardef_name vardef =
  Node.VarDef.(to_tuple vardef |> fst |> Identifier.to_string |> escape_name)

let parameter_string parameters =
  let parameters = List.map vardef_name parameters in
  String.concat ", " parameters

let emit_function recursively_emit name_generator parameters body =
  let parameters = parameter_string parameters in
  let body_expression = recursively_emit name_generator body in
  let body_expression = Lua_fragment.lua_string body_expression in
  let function_expression = sprintf "function(%s)\n%s\nend" parameters body_expression in
  Lua_fragment.make_expression function_expression

let emit_if recursively_emit name_generator test_expression if_expression else_expression =
  let (name_generator, generated_name) = Name_gen.next_value name_generator in
  let expression_target = sprintf "%s =" generated_name in
  let test_expression = recursively_emit name_generator test_expression in
  let test_result = Lua_fragment.result_expression test_expression in
  let if_fragment = recursively_emit name_generator if_expression in
  let if_string = Lua_fragment.lua_string ~target:expression_target if_fragment in
  let else_fragment = recursively_emit name_generator else_expression in
  let else_string = Lua_fragment.lua_string ~target:expression_target else_fragment in
  let combined_string = String.concat "\n" [
    sprintf "%s nil\nif %s then" expression_target test_result;
    sprintf "%s\nelse\n%s\nend" if_string else_string
  ] in
  let if_statement = Lua_fragment.make_result_statement generated_name combined_string in
  Lua_fragment.insert_preamble if_statement test_expression

let build_binding_statements recursively_emit bindings =
  List.map (fun binding ->
    let (name, expression) = Node.Binding.to_tuple binding in
    let name = Identifier.to_string name |> escape_name in
    let result = sprintf "%s =" name in
    Lua_fragment.lua_string ~target:result (recursively_emit expression)) bindings

let emit_let recursively_emit name_generator bindings body =
  let (name_generator, generated_name) = Name_gen.next_value name_generator in
  let expression_target = sprintf "%s =" generated_name in
  let binding_strings = build_binding_statements (recursively_emit name_generator) bindings in
  let binding_string = String.concat "\n" binding_strings in
  let body_fragment = recursively_emit name_generator body in
  let body_string = Lua_fragment.lua_string ~target:expression_target body_fragment in
  let combined_string = sprintf "%s nil\ndo\n%s\n%s\nend" expression_target binding_string body_string in
  Lua_fragment.make_result_statement generated_name combined_string

let build_argument_statements recursively_emit arguments =
  let rec emit_arguments' argument_expressions = function
    | [] -> argument_expressions
    | argument :: rest -> emit_arguments' (recursively_emit argument :: argument_expressions) rest in
  List.rev (emit_arguments' [] arguments)

let build_arguments recursively_emit arguments =
  let argument_expressions = build_argument_statements recursively_emit arguments in
  let result_expressions = List.map Lua_fragment.result_expression argument_expressions in
  (result_expressions, argument_expressions)

let emit_infix_apply recursively_emit operator arguments =
  let operator = Lua_fragment.result_expression (emit_symbol ~wrap_ops:false operator) in
  let (result_expressions, argument_expressions) = build_arguments recursively_emit arguments in
  let (left, right) =
    match result_expressions with
    | first :: second :: [] -> (first, second)
    | _ -> assert false in
  let application = sprintf "(%s %s %s)" left operator right in
  let application = Lua_fragment.make_expression application in
  List.fold_left Lua_fragment.insert_preamble application argument_expressions

let emit_prefix_apply recursively_emit function_name arguments =
  let function_name = Lua_fragment.result_expression (emit_symbol function_name) in
  let (result_expressions, argument_expressions) = build_arguments recursively_emit arguments in
  let arguments = String.concat ", " result_expressions in
  let application = sprintf "%s(%s)" function_name arguments in
  let application = Lua_fragment.make_expression application in
  List.fold_left Lua_fragment.insert_preamble application argument_expressions

let emit_literal_apply recursively_emit name_generator function_literal arguments =
  let (name_generator, generated_name) = Name_gen.next_value name_generator in
  let function_expression = Lua_fragment.result_expression (recursively_emit name_generator function_literal) in
  let function_assignment = sprintf "%s = %s" generated_name function_expression in
  let function_preamble = Lua_fragment.make_result_statement generated_name function_assignment in
  let (result_expressions, argument_expressions) = build_arguments (recursively_emit name_generator) arguments in
  let arguments = String.concat ", " result_expressions in
  let application = sprintf "%s(%s)" generated_name arguments in
  let application = Lua_fragment.make_expression application in
  let preamble = (function_preamble :: argument_expressions) in
  List.fold_left Lua_fragment.insert_preamble application preamble

let emit_apply recursively_emit name_generator callable arguments =
  match callable with
  | Node.Symbol (name, _) when is_infix_operator name ->
      emit_infix_apply (recursively_emit name_generator) name arguments
  | Node.Symbol (name, _) ->
      emit_prefix_apply (recursively_emit name_generator) name arguments
  | Node.Fn (p, r, b, m) ->
      emit_literal_apply recursively_emit name_generator (Node.Fn (p, r, b, m)) arguments
  | _ -> assert false

let build_assign_statements generated_name recursively_emit bindings =
  List.map (fun binding ->
    let (name, expression) = Node.Binding.to_tuple binding in
    let name = Identifier.to_string name |> escape_name in
    let result = sprintf "%s.%s =" generated_name name in
    Lua_fragment.lua_string ~target:result (recursively_emit expression)) bindings

let emit_cons recursively_emit name_generator bindings =
  let (name_generator, generated_name) = Name_gen.next_value name_generator in
  let binding_strings = build_assign_statements generated_name (recursively_emit name_generator) bindings in
  let binding_string = String.concat "\n" binding_strings in
  let combined_string = sprintf "%s = {}\ndo\n%s\nend" generated_name binding_string in
  Lua_fragment.make_result_statement generated_name combined_string

let emit_get record field =
  match record with
  | Node.Symbol (name, _) -> begin
    let record = emit_name ~wrap_ops:false name in
    let field = Identifier.to_string field in
    Lua_fragment.make_expression (sprintf "%s.%s" record field)
  end
  | _ -> assert false

let emit_set recursively_emit name_generator record field expression =
  match record with
  | Node.Symbol (name, _) -> begin
    let record = emit_name ~wrap_ops:false name in
    let field = Identifier.to_string field in
    let expression_fragment = recursively_emit name_generator expression in
    let result_expression = Lua_fragment.result_expression expression_fragment in
    let assignment = sprintf "%s.%s = %s" record field result_expression in
    let assignment = Lua_fragment.make_unit_statement assignment in
    Lua_fragment.insert_preamble assignment expression_fragment
  end
  | _ -> assert false

let rec emit_node name_generator node =
  let recursively_emit = emit_node in
  match node with
  | Node.NumLit (value, _) -> emit_number value
  | Node.StrLit (value, _) -> emit_string value
  | Node.Symbol (value, _) -> emit_symbol value
  | Node.Rec _ -> Lua_fragment.make_expression ""
  | Node.Def (name, expression, _) ->
      emit_def recursively_emit name_generator name expression
  | Node.Fn (parameters, _, body, _) ->
      emit_function recursively_emit name_generator parameters body
  | Node.If (test_expression, if_expression, else_expression, _) ->
      emit_if recursively_emit name_generator test_expression if_expression else_expression
  | Node.Let (bindings, expression, _) ->
      emit_let recursively_emit name_generator bindings expression
  | Node.Apply (callable, arguments, _) ->
      emit_apply recursively_emit name_generator callable arguments
  | Node.Cons (_, bindings, _) ->
      emit_cons recursively_emit name_generator bindings
  | Node.Get (record, field, _) ->
      emit_get record field
  | Node.Set (record, field, expression, _) ->
      emit_set recursively_emit name_generator record field expression
  | Node.Cast (_, expression, _) ->
      recursively_emit name_generator expression
  | Node.Type _ -> assert false

let emit_typed_node (node, _) =
  let generator = Name_gen.generator in
  Lua_fragment.lua_string ~target:"" (emit_node generator node)

let emit nodes = List.map emit_typed_node nodes
