open Printf

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
        Result.map (fun e -> Node.Def (name, e)) expr
  | _ -> Error { CompileError.message = "invalid DEF form" }

let analyze_params params =
  let analyze_param = function
    | Form.Symbol p -> Ok (Node.Param.from_string p)
    | _ -> Error { CompileError.message = "fn param not a symbol" } in
  let fold_fn param prior =
    Result.flat_map (fun ps ->
      Result.map (fun p -> p :: ps) (analyze_param param)
    ) prior in
  List.fold_right fold_fn params (Ok [])

let analyze_fn f_analyze = function
  | Form.List raw_params :: raw_body :: [] ->
      let params = analyze_params raw_params
      and expr = f_analyze raw_body in
        Result.flat_map (fun p ->
          Result.map (fun e -> Node.Fn (p, e)) expr
          ) params
  | _ -> Error { CompileError.message = "invalid FN form" }

let analyze_op f_analyze op (args: Form.t list) =
  if op = "def" then analyze_def f_analyze args
  else if op = "fn" then analyze_fn f_analyze args
  else Error { CompileError.message = sprintf "no special form '%s'" op }

let analyze_list analyze_fn = function
  | Form.Symbol op :: args -> analyze_op analyze_fn op args
  | op :: args -> Error { CompileError.message = "functions unsupported" }
  | _ -> Error { CompileError.message = "unexpected ()" }

let rec analyze_form = function
  | Form.Number n -> Ok (Node.NumLit n)
  | Form.String s -> Ok (Node.StrLit s)
  | Form.Symbol s -> Ok (Node.SymLit s)
  | Form.List l -> analyze_list analyze_form l
  | f -> Error { CompileError.message = Form.to_string f }

let analyze modul forms = List.map analyze_form forms
