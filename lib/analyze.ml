open Printf
open Result
open Extensions

module CompileError = struct
  type t = {
    message: string;
  }

  let message e = e.message
  let to_string e = "CompileError: " ^ message e
end

let analyze_def f_analyze = function
  | Form.Symbol raw_name :: raw_expr :: [] ->
      let name = Module.Var.Name.from_string raw_name
      and expr = f_analyze raw_expr in
        expr >>= fun e ->
        return (Node.Def (name, e))
  | _ -> Error { CompileError.message = "invalid DEF form" }

let analyze_params params =
  let analyze_param = function
    | Form.Symbol p -> Ok (Node.Param.from_string p)
    | _ -> Error { CompileError.message = "fn param not a symbol" } in
  let fold_fn param prior =
    prior >>= fun ps ->
    (analyze_param param) >>= fun p ->
    return (p :: ps) in
  List.fold_right fold_fn params (Ok [])

let analyze_fn f_analyze = function
  | Form.List raw_params :: raw_body :: [] ->
      let params = analyze_params raw_params
      and expr = f_analyze raw_body in
      params >>= fun p ->
      expr >>= fun e ->
      return (Node.Fn (p, e))
  | _ -> Error { CompileError.message = "invalid FN form" }

let analyze_if f_analyze = function
  | raw_test :: raw_if :: raw_else :: [] ->
      (f_analyze raw_test) >>= fun t ->
      (f_analyze raw_if) >>= fun i ->
      (f_analyze raw_else) >>= fun e ->
      return (Node.If (t, i, e))
  | _ -> Error { CompileError.message = "invalid IF form" }

let analyze_bindings f_analyze bindings =
  let analyze_binding = function
    | (Form.Symbol b, raw_expr) ->
        let name = Node.Binding.Name.from_string b in
        (f_analyze raw_expr) >>= fun e ->
        return (Node.Binding.from_node name e)
    | _ -> Error { CompileError.message = "invalid BINDING form" } in
  let fold_fn binding prior =
    prior >>= fun bs ->
    (analyze_binding binding) >>= fun b ->
    return (b :: bs) in
  if (List.length bindings mod 2) = 0 then
    List.fold_right fold_fn (List.as_pairs bindings) (Ok [])
  else Error { CompileError.message = "invalid BINDING form" }

let analyze_let f_analyze = function
  | Form.List bindings :: body :: [] ->
      let bindings = analyze_bindings f_analyze bindings in
      bindings >>= fun bi ->
      (f_analyze body) >>= fun b ->
      return (Node.Let (bi, b))
  | _ -> Error { CompileError.message = "invalid LET form" }

let analyze_op f_analyze op (args: Form.t list) =
  if op = "def" then analyze_def f_analyze args
  else if op = "fn" then analyze_fn f_analyze args
  else if op = "if" then analyze_if f_analyze args
  else if op = "let" then analyze_let f_analyze args
  else Error { CompileError.message = sprintf "no special form '%s'" op }

let analyze_list f_analyze = function
  | Form.Symbol op :: args -> analyze_op f_analyze op args
  | op :: args -> Error { CompileError.message = "functions unsupported" }
  | _ -> Error { CompileError.message = "unexpected ()" }

let rec analyze_form = function
  | Form.Number n -> Ok (Node.NumLit n)
  | Form.String s -> Ok (Node.StrLit s)
  | Form.Symbol s -> Ok (Node.SymLit s)
  | Form.List l -> analyze_list analyze_form l

let analyze modul forms = List.map analyze_form forms
