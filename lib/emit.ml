open Printf

module State = struct
  let var_prefix = "__var"
  let fn_prefix = "__fn"

  type t = {
    var_id: int;
  }

  let make () = { var_id = 0; }

  let var { var_id; } = sprintf "%s%d" var_prefix var_id

  let new_var state = { var_id = state.var_id + 1 }

end

let is_simple = function
  | Node.If _ | Node.Let _ -> false
  | _ -> true

let emit_simple recur_fn state expr =
  let var = State.var state in
  sprintf "%s = %s" var (recur_fn state expr)

let emit_complex recur_fn state expr =
  let next_var = State.var (State.new_var state)
  and var = State.var state in
  String.concat "\n" [
    recur_fn state expr;
    sprintf "%s = %s" var next_var
  ]

let emit_expr recur_fn state expr =
  if is_simple expr then
    emit_simple recur_fn state expr
  else
    emit_complex recur_fn state expr

let emit_num n =
  sprintf "%f" n

let emit_str s =
  sprintf "\"%s\"" s

let emit_sym = function
  | Name.Local s -> s
  | Name.Module (_, vn) -> Module.Var.Name.to_string vn

let emit_def recur_fn state var expr =
  let state = State.new_var state
  and (name, _) = Node.VarDef.to_tuple var in
  let name = Node.VarDef.Name.to_string name in
  if is_simple expr then
    sprintf "local %s = %s" name (recur_fn state expr)
  else
    String.concat "\n" [
      sprintf "local %s" (State.var state);
      emit_complex recur_fn state expr;
      sprintf "local %s = %s" name (State.var state);
    ]

let emit_fn recur_fn state params body =
  let state = State.new_var state in
  let map_fn var_def =
    let (name, _) = Node.VarDef.to_tuple var_def in
    Node.VarDef.Name.to_string name in
  let params = List.map map_fn params in
  let params = String.concat ", " params in
  String.concat "\n" [
    sprintf "function(%s)" params;
    sprintf "local %s" (State.var state);
    emit_expr recur_fn state body;
    sprintf "return %s" (State.var state);
    sprintf "end";
  ]

let emit_simple_test recur_fn state test =
  sprintf "if %s then" (recur_fn state test)

let emit_complex_test recur_fn state test =
  let next_var = State.var (State.new_var state) in
  String.concat "\n" [
    recur_fn state test;
    sprintf "if %s then" next_var;
  ]

let emit_test recur_fn state test =
  if is_simple test then
    emit_simple_test recur_fn state test
  else
    emit_complex_test recur_fn state test

let emit_if recur_fn state tst iff els =
  let state = State.new_var state in
  String.concat "\n" [
    sprintf "local %s" (State.var state);
    emit_test recur_fn state tst;
    emit_expr recur_fn state iff;
    sprintf "else";
    emit_expr recur_fn state els;
    sprintf "end"
  ]

let emit_simple_binding recur_fn state name expr =
  let name = Node.Binding.Name.to_string name in
  sprintf "local %s = %s" name (recur_fn state expr)

let emit_complex_binding recur_fn state name expr =
  let next_state = State.new_var state
  and name = Node.Binding.Name.to_string name in
  String.concat "\n" [
    recur_fn state expr;
    sprintf "local %s = %s" name (State.var next_state)
  ]

let emit_binding recur_fn state binding =
  let (name, expr) = Node.Binding.to_tuple binding in
  if is_simple expr then
    emit_simple_binding recur_fn state name expr
  else
    emit_complex_binding recur_fn state name expr

let emit_bindings recur_fn state bindings =
  let map_fn = emit_binding recur_fn state in
  String.concat "\n" (List.map map_fn bindings)

let emit_let recur_fn state bindings body =
  let state = State.new_var state in
  String.concat "\n" [
    sprintf "local %s" (State.var state);
    sprintf "do";
    emit_bindings recur_fn state bindings;
    emit_expr recur_fn state body;
    sprintf "end";
  ]

let emit_args recur_fn state args =
  let fold_fn (args, emitted, state) arg =
    let expr = recur_fn state arg in
    if is_simple arg then
      ((expr :: args), emitted, state)
    else
      let next_state = State.new_var state in
      let arg = State.var next_state in
      ((arg :: args), (expr :: emitted), next_state)
  and initial = ([], [], state) in
  let (args, emitted, _) = List.fold_left fold_fn initial args in
  (args, emitted)

(* TODO emit code for nested complex fn application *)
(* TODO emit code for application of anonymous functions *)
let emit_apply recur_fn state fn args =
  let fn = recur_fn state fn
  and (args, emitted) = emit_args recur_fn state args in
  let args = String.concat ", " (List.rev args) in
  let fn_call = sprintf "%s(%s)" fn args in
  String.concat "\n" (List.rev (fn_call :: emitted))

let emit_cast recur_fn state tdef expr =
  recur_fn state expr

let rec emit_node state = function
  | Node.NumLit n -> emit_num n
  | Node.StrLit s -> emit_str s
  | Node.SymLit s -> emit_sym s
  | Node.Def (var, expr) -> emit_def emit_node state var expr
  | Node.Fn (params, body) -> emit_fn emit_node state params body
  | Node.If (tst, iff, els) -> emit_if emit_node state tst iff els
  | Node.Let (bindings, body) -> emit_let emit_node state bindings body
  | Node.Apply (fn, args) -> emit_apply emit_node state fn args
  | Node.Cast (tdef, expr) -> emit_cast emit_node state tdef expr

let emit nodes =
  let state = State.make () in
  List.map (fun n -> emit_node state (fst n)) nodes
