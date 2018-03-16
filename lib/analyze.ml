open Printf

module CompileError = struct
  type t = {
    message: string;
  }

  let message e = e.message
  let to_string e = "CompileError: " ^ message e
end

let analyze_def analyze_fn = function
  | Form.Symbol raw_name :: raw_expr :: [] ->
      let name = Module.Var.Name.from_string raw_name
      and expr = analyze_fn raw_expr in
        Result.map (fun e -> Node.Def (name, e)) expr
  | _ -> Error { CompileError.message = "Invalid Def Form" }

let analyze_op analyze_fn op (args: Form.t list) =
  if op = "def" then analyze_def analyze_fn args
  else Error { CompileError.message = sprintf "No special form '%s'" op }

let analyze_list analyze_fn = function
  | Form.Symbol op :: args -> analyze_op analyze_fn op args
  | op :: args -> Error { CompileError.message = "Functions unsupported" }
  | _ -> Error { CompileError.message = "Unexpected ()" }

let rec analyze_form = function
  | Form.Number n -> Ok (Node.NumLit n)
  | Form.String s -> Ok (Node.StrLit s)
  | Form.Symbol s -> Ok (Node.SymLit s)
  | Form.List l -> analyze_list analyze_form l
  | f -> Error { CompileError.message = Form.to_string f }

let analyze modul forms = List.map analyze_form forms
