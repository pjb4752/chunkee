open Lex
open Printf
open Thwack.Result
open Thwack.Extensions

module Node = Ast.Parsed_node

let parse_name_expr name =
  if String.contains name '.' then
    match String.split_on_char '/' name with
    | mod_name :: name :: [] -> begin
      let mod_parts = String.split_on_char '.' mod_name in
      let mod_parts = List.map (Mod_name.Name.from_string) mod_parts in
      match List.rev mod_parts with
      | mod_name :: path_parts -> begin
        let mod_path = Mod_name.Path.from_list path_parts in
        let mod_name = Mod_name.make mod_path mod_name in
        Ok (Name_expr.QualName (mod_name, name))
      end
      | _ -> Error (Cmpl_err.ParseError "unrecognized symbol format")
    end
    | _ -> Error (Cmpl_err.ParseError "unrecognized symbol format")
  else
    Ok (Name_expr.BareName name)

let rec parse_type = function
  | Form.Symbol t -> Ok (Type_ref.from_string t)
  | Form.Vec ts -> begin
    let fold_fn t ts =
      ts >>= fun ts ->
      (parse_type t) >>= fun t ->
      return (t :: ts) in
   match List.fold_right fold_fn ts (Ok []) with
   | Error e -> Error e
   | Ok [] -> Error (Cmpl_err.ParseError "invalid TYPE form")
   | Ok ts -> Ok (Type_ref.from_list ts)
  end
  | _ -> Error (Cmpl_err.ParseError "invalid TYPE form")

let parse_var_def = function
  | Form.Vec (Form.Symbol n :: t :: []) -> begin
      (parse_type t) >>= fun t ->
        let n = Node.VarDef.Name.from_string n in return (n, t)
  end
  | _ -> Error (Cmpl_err.ParseError "invalid VAR form")

let parse_rec_fields f_parse fields =
  let fold_fn (n, t) prior =
    prior >>= fun fs ->
      (parse_var_def (Form.Vec ([n; t]))) >>= fun (n, t) ->
      let field = Node.VarDef.from_parts n t in
      return (field :: fs) in
  if (List.length fields mod 2) = 0 then
    List.fold_right fold_fn (List.as_pairs fields) (Ok [])
  else Error (Cmpl_err.ParseError "invalid FIELDS form")

let parse_rec f_parse = function
  | Form.Symbol name :: Form.Vec fields :: [] ->
      let name = Node.Name.from_string name in
      let fields = parse_rec_fields f_parse fields in
      fields >>= fun fs ->
      return (Node.Rec (name, fs))
  | _ -> Error (Cmpl_err.ParseError "invalid RECORD form")

let parse_def f_parse = function
  | Form.Symbol name :: expr :: [] ->
      (f_parse expr) >>= fun expr ->
      let name = Node.Name.from_string name in
      return (Node.Def (name, expr))
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
  | Form.Vec bindings :: body :: [] ->
      let bindings = parse_bindings f_parse bindings in
      bindings >>= fun bi ->
      (f_parse body) >>= fun b ->
      return (Node.Let (bi, b))
  | _ -> Error (Cmpl_err.ParseError "invalid LET form")

let parse_cast f_parse = function
  | raw_type :: expr :: [] -> begin
    (parse_type raw_type) >>= fun t ->
    (f_parse expr) >>= fun e ->
    return (Node.Cast (t, e))
  end
  | _ -> Error (Cmpl_err.ParseError "invalid CAST form")

let parse_args f_parse args =
  let fold_fn arg prior =
    prior >>= fun args ->
    (f_parse arg) >>= fun a ->
    return (a :: args) in
  List.fold_right fold_fn args (Ok [])

let parse_num_apply f_parse num args =
  (parse_args f_parse args) >>= fun args ->
  return (Node.Apply (Node.NumLit num, args))

let parse_str_apply f_parse str args =
  (parse_args f_parse args) >>= fun args ->
  return (Node.Apply (Node.StrLit str, args))

let parse_sym_apply f_parse fn args =
  (parse_name_expr fn) >>= fun fn ->
  (parse_args f_parse args) >>= fun args ->
  return (Node.Apply (Node.SymLit fn, args))

let parse_fn_apply f_parse fn args =
  (f_parse (Form.List fn)) >>= fun fn ->
  (parse_args f_parse args) >>= fun args ->
  return (Node.Apply (fn, args))

let parse_op f_parse op (args: Form.t list) =
  if op = "defrec" then parse_rec f_parse args
  else if op = "def" then parse_def f_parse args
  else if op = "fn" then parse_fn f_parse args
  else if op = "if" then parse_if f_parse args
  else if op = "let" then parse_let f_parse args
  else if op = "cast" then parse_cast f_parse args
  else parse_sym_apply f_parse op args

let parse_symbol symbol =
  (parse_name_expr symbol) >>= fun symbol ->
  return (Node.SymLit symbol)

let parse_list f_parse = function
  | Form.Number n :: args -> parse_num_apply f_parse n args
  | Form.String s :: args -> parse_str_apply f_parse s args
  | Form.Symbol op :: args -> parse_op f_parse op args
  | Form.List expr :: args -> parse_fn_apply f_parse expr args
  | _ -> Error (Cmpl_err.ParseError "unrecognized form")

let rec parse_form = function
  | Form.Number n -> Ok (Node.NumLit n)
  | Form.String s -> Ok (Node.StrLit s)
  | Form.Symbol s -> parse_symbol s
  | Form.List l -> parse_list parse_form l
  | _ -> Error (Cmpl_err.ParseError "unrecognized form")

let parse forms =
  let fold_fn form forms =
    forms >>= fun fs ->
    (parse_form form) >>= fun f ->
    return (f :: fs) in
  List.fold_right fold_fn forms (Ok [])
