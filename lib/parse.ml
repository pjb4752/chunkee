open Lex
open Printf
open Result
open Extensions

type t = string Node.t

let rec parse_type = function
  | Form.Symbol t -> Ok (Node.VarDef.Type.from_string t)
  | Form.Vec ts -> begin
    let fold_fn t ts =
      ts >>= fun ts ->
      (parse_type t) >>= fun t ->
      return (t :: ts) in
   match List.fold_right fold_fn ts (Ok []) with
   | Error e -> Error e
   | Ok [] -> Error (Cmpl_err.ParseError "invalid TYPE form")
   | Ok ts -> Ok (Node.VarDef.Type.from_list ts)
  end
  | _ -> Error (Cmpl_err.ParseError "invalid TYPE form")

let parse_var_def = function
  | Form.Vec (Form.Symbol n :: t :: []) -> begin
    (parse_type t) >>= fun t ->
      let n = Node.VarDef.Name.from_string n in return (n, t)
  end
  | _ -> Error (Cmpl_err.ParseError "invalid VAR form")

let parse_def f_parse = function
  | raw_def :: raw_expr :: [] ->
      (parse_var_def raw_def) >>= fun (n, t) ->
      (f_parse raw_expr) >>= fun e ->
      let var = Node.VarDef.from_parts n t in
      return (Node.Def (var, e))
  | _ -> Error (Cmpl_err.ParseError "invalid DEF form")

let parse_params params =
  let fold_fn param prior =
    prior >>= fun ps ->
    (parse_var_def param) >>= fun (n, t) ->
    let p = Node.VarDef.from_parts n t in
    return (p :: ps) in
  List.fold_right fold_fn params (Ok [])

let parse_fn f_parse = function
  | Form.Vec raw_params :: raw_body :: [] ->
      let params = parse_params raw_params
      and expr = f_parse raw_body in
      params >>= fun p ->
      expr >>= fun e ->
      return (Node.Fn (p, e))
  | _ -> Error (Cmpl_err.ParseError "invalid FN form")

let parse_if f_parse = function
  | raw_test :: raw_if :: raw_else :: [] ->
      (f_parse raw_test) >>= fun t ->
      (f_parse raw_if) >>= fun i ->
      (f_parse raw_else) >>= fun e ->
      return (Node.If (t, i, e))
  | _ -> Error (Cmpl_err.ParseError "invalid IF form")

let parse_bindings f_parse bindings =
  let parse_binding = function
    | (Form.Symbol b, raw_expr) ->
        let name = Node.Binding.Name.from_string b in
        (f_parse raw_expr) >>= fun e ->
        return (Node.Binding.from_node name e)
    | _ -> Error (Cmpl_err.ParseError "invalid BINDING form") in
  let fold_fn binding prior =
    prior >>= fun bs ->
    (parse_binding binding) >>= fun b ->
    return (b :: bs) in
  if (List.length bindings mod 2) = 0 then
    List.fold_right fold_fn (List.as_pairs bindings) (Ok [])
  else Error (Cmpl_err.ParseError "invalid BINDING form")

let parse_let f_parse = function
  | Form.List bindings :: body :: [] ->
      let bindings = parse_bindings f_parse bindings in
      bindings >>= fun bi ->
      (f_parse body) >>= fun b ->
      return (Node.Let (bi, b))
  | _ -> Error (Cmpl_err.ParseError "invalid LET form")

let parse_args f_parse args =
  let fold_fn arg prior =
    prior >>= fun args ->
    (f_parse arg) >>= fun a ->
    return (a :: args) in
  List.fold_right fold_fn args (Ok [])

let parse_apply f_parse fn args =
  (parse_args f_parse args) >>= fun args ->
  return (Node.Apply (Node.SymLit fn, args))

let parse_op f_parse op (args: Form.t list) =
  if op = "def" then parse_def f_parse args
  else if op = "fn" then parse_fn f_parse args
  else if op = "if" then parse_if f_parse args
  else if op = "let" then parse_let f_parse args
  else parse_apply f_parse op args

let parse_list f_parse = function
  | Form.Symbol op :: args -> parse_op f_parse op args
  | op :: args -> Error (Cmpl_err.ParseError "no first-class functions")
  | _ -> Error (Cmpl_err.ParseError "unexpected ()")

let rec parse_form = function
  | Form.Number n -> Ok (Node.NumLit n)
  | Form.String s -> Ok (Node.StrLit s)
  | Form.Symbol s -> Ok (Node.SymLit s)
  | Form.List l -> parse_list parse_form l
  | Form.Vec v -> assert false

let parse forms =
  let fold_fn form forms =
    forms >>= fun fs ->
    (parse_form form) >>= fun f ->
    return (f :: fs) in
  List.fold_right fold_fn forms (Ok [])
