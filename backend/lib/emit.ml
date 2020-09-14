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

let is_infix_op = function
  | Name.Var.Module (mod_name, var_name) -> begin
    let var_name = Identifier.to_string var_name in
    match Stdlib.find_lua_module mod_name with
    | Some modul -> Lua_module.operator_exists modul var_name
    | None -> false
  end
  | Name.Var.Local _ -> false

let emit_mod_alias parts = String.concat "_" parts

let escape_char chr =
  Option.value (Char_map.find_opt chr char_maps) ~default:[chr]

let escape_chars chars =
  List.fold_left (fun accum chr ->
    escape_char chr |> List.append accum) [] chars

let escape_name name =
  String.to_chars name |> escape_chars |> String.from_chars

let emit_require module_name =
  let segments = Module_name.to_segments module_name in
  let segments = List.map Module_name.Segment.to_string segments in
  let alias = emit_mod_alias segments in
  let require_path = String.concat "." segments in
  sprintf "%s = require 'flopcore.%s'" alias require_path

let emit_num num =
  Lua_snippet.make_expr (sprintf "%f" num)

let emit_str str =
  Lua_snippet.make_expr (sprintf "\"%s\"" str)

let find_operator mod_name var_name =
  Option.(
    (Stdlib.find_lua_module mod_name) >>= fun modul ->
    (Lua_module.find_operator modul var_name) >>= fun operator ->
    return operator)

let emit_name ?wrap_ops:(wrap_ops=true) = function
  | Name.Var.Local name -> escape_name name
  | Name.Var.Module (mod_name, var_name) ->
    let var_name = Identifier.to_string var_name in
    match find_operator mod_name var_name with
    | Some operator when wrap_ops -> Lua_operator.wrapper operator
    | Some operator -> Lua_operator.name operator
    | None -> escape_name var_name

let emit_sym ?wrap_ops:(wrap_ops=true) symbol =
  Lua_snippet.make_expr @@ emit_name ~wrap_ops:wrap_ops symbol

let emit_def fn generator name expr =
  let name = Identifier.to_string name |> escape_name in
  let expr_snippet = fn generator expr in
  let result_expr = Lua_snippet.result_expr expr_snippet in
  let assignment = sprintf "%s = %s" name result_expr in
  let def_stmt = Lua_snippet.make_unit_stmt assignment in
  Lua_snippet.insert_preamble def_stmt expr_snippet

let vardef_name vardef =
  Node.VarDef.(to_tuple vardef |> fst |> Identifier.to_string |> escape_name)

let param_str params =
  let params = List.map vardef_name params in
  String.concat ", " params

let emit_fn fn generator params body =
  let params = param_str params and body_expr = fn generator body in
  let body_expr = Lua_snippet.lua_string body_expr in
  let fn_expr = sprintf "function(%s)\n%s\nend" params body_expr in
  Lua_snippet.make_expr fn_expr

let emit_if fn generator tst iff els =
  let (generator, result_var) = Name_gen.next_value generator in
  let target = sprintf "%s =" result_var in
  let tst_expr = fn generator tst in
  let tst_result = Lua_snippet.result_expr tst_expr in
  let iff_str = Lua_snippet.lua_string ~target:target (fn generator iff) in
  let els_str = Lua_snippet.lua_string ~target:target (fn generator els) in
  let if_str = String.concat "\n" [
    sprintf "%s nil\nif %s then" target tst_result;
    sprintf "%s\nelse\n%s\nend" iff_str els_str
  ] in
  let if_stmt = Lua_snippet.make_result_stmt result_var if_str in
  Lua_snippet.insert_preamble if_stmt tst_expr

let build_binding_stmts fn bindings =
  List.map (fun b ->
    let (name, expr) = Node.Binding.to_tuple b in
    let name = Identifier.to_string name |> escape_name in
    let result = sprintf "%s =" name in
    Lua_snippet.lua_string ~target:result (fn expr)) bindings

let emit_let fn generator bindings expr =
  let (generator, result_var) = Name_gen.next_value generator in
  let target = sprintf "%s =" result_var in
  let bind_strs = build_binding_stmts (fn generator) bindings in
  let bind_str = String.concat "\n" bind_strs in
  let expr_str = Lua_snippet.lua_string ~target:target (fn generator expr) in
  let let_str = sprintf "%s nil\ndo\n%s\n%s\nend" target bind_str expr_str in
  Lua_snippet.make_result_stmt result_var let_str

let build_arg_stmts fn args =
  let rec emit_args' arg_exprs = function
    | [] -> arg_exprs
    | arg :: args -> emit_args' (fn arg :: arg_exprs) args in
  List.rev (emit_args' [] args)

let build_args fn args =
  let arg_exprs = build_arg_stmts fn args in
  let result_exprs = List.map Lua_snippet.result_expr arg_exprs in
  (result_exprs, arg_exprs)

let emit_infix_apply fn operator args =
  let operator = Lua_snippet.result_expr (emit_sym ~wrap_ops:false operator) in
  let (result_exprs, arg_exprs) = build_args fn args in
  let (left, right) =
    match result_exprs with
    | res1 :: res2 :: [] -> (res1, res2)
    | _ -> assert false in
  let application = sprintf "(%s %s %s)" left operator right in
  let application = Lua_snippet.make_expr application in
  List.fold_left Lua_snippet.insert_preamble application arg_exprs

let emit_prefix_apply fn name args =
  let fn_name = Lua_snippet.result_expr (emit_sym name) in
  let (result_exprs, arg_exprs) = build_args fn args in
  let arguments = String.concat ", " result_exprs in
  let application = sprintf "%s(%s)" fn_name arguments in
  let application = Lua_snippet.make_expr application in
  List.fold_left Lua_snippet.insert_preamble application arg_exprs

let emit_literal_apply fn generator fn_lit args =
  let (generator, result_var) = Name_gen.next_value generator in
  let fn_expr = Lua_snippet.result_expr (fn generator fn_lit) in
  let fn_assignment = sprintf "%s = %s" result_var fn_expr in
  let fn_preamble = Lua_snippet.make_result_stmt result_var fn_assignment in
  let (result_exprs, arg_exprs) = build_args (fn generator) args in
  let arguments = String.concat ", " result_exprs in
  let application = sprintf "%s(%s)" result_var arguments in
  let application = Lua_snippet.make_expr application in
  let preamble = (fn_preamble :: arg_exprs) in
  List.fold_left Lua_snippet.insert_preamble application preamble

let emit_apply fn generator callable args =
  match callable with
  | Node.Symbol (n, _) when is_infix_op n -> emit_infix_apply (fn generator) n args
  | Node.Symbol (n, _) -> emit_prefix_apply (fn generator) n args
  | Node.Fn (p, r, b, m) -> emit_literal_apply fn generator (Node.Fn (p, r, b, m)) args
  (*| Node.Apply (f, a) -> emit_apply fn f a*)
  | _ -> assert false

let build_assign_stmts result_var fn bindings =
  List.map (fun b ->
    let (name, expr) = Node.Binding.to_tuple b in
    let name = Identifier.to_string name |> escape_name in
    let result = sprintf "%s.%s =" result_var name in
    Lua_snippet.lua_string ~target:result (fn expr)) bindings

let emit_cons fn generator bindings =
  let (generator, result_var) = Name_gen.next_value generator in
  let bind_strs = build_assign_stmts result_var (fn generator) bindings in
  let bind_str = String.concat "\n" bind_strs in
  let let_str = sprintf "%s = {}\ndo\n%s\nend" result_var bind_str in
  Lua_snippet.make_result_stmt result_var let_str

let emit_get record field =
  match record with
  | Node.Symbol (name, _) -> begin
    let record = emit_name ~wrap_ops:false name in
    let field = Identifier.to_string field in
    Lua_snippet.make_expr (sprintf "%s.%s" record field)
  end
  | _ -> assert false

let emit_set fn generator record field expr =
  match record with
  | Node.Symbol (name, _) -> begin
    let record = emit_name ~wrap_ops:false name in
    let field = Identifier.to_string field in
    let expr_snippet = fn generator expr in
    let result_expr = Lua_snippet.result_expr expr_snippet in
    let assignment = sprintf "%s.%s = %s" record field result_expr in
    let assignment = Lua_snippet.make_unit_stmt assignment in
    Lua_snippet.insert_preamble assignment expr_snippet
  end
  | _ -> assert false

let rec emit_node generator = function
  | Node.NumLit (num, _) -> emit_num num
  | Node.StrLit (str, _) -> emit_str str
  | Node.Symbol (sym, _) -> emit_sym sym
  | Node.Rec _ -> Lua_snippet.make_expr ""
  | Node.Def (name, expr, _) -> emit_def emit_node generator name expr
  | Node.Fn (params, _, body, _) -> emit_fn emit_node generator params body
  | Node.If (tst, iff, els, _) -> emit_if emit_node generator tst iff els
  | Node.Let (bindings, expr, _) -> emit_let emit_node generator bindings expr
  | Node.Apply (callable, args, _) -> emit_apply emit_node generator callable args
  | Node.Cons (_, bindings, _) -> emit_cons emit_node generator bindings
  | Node.Get (record, field, _) -> emit_get record field
  | Node.Set (record, field, expr, _) -> emit_set emit_node generator record field expr
  | Node.Cast (_, expr, _) -> emit_node generator expr
  | Node.Type _ -> assert false

let emit_typed_node (node, _) =
  let generator = Name_gen.generator in
  Lua_snippet.lua_string ~target:"" (emit_node generator node)

let emit nodes = List.map emit_typed_node nodes
