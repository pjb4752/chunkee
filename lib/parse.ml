open Lex
open Printf
open Thwack.Result
open Thwack.Extensions

module Node = Ast.Parsed_node

let invalid_symbol_error = Error (Cmpl_err.ParseError "invalid SYMBOL form")

let invalid_type_error = Error (Cmpl_err.ParseError "invalid TYPE form")

let parse_qual_name name =
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
    | _ -> invalid_symbol_error
  end
  | _ -> invalid_symbol_error

let parse_name_expr name =
  if String.contains name '.' then parse_qual_name name
  else Ok (Name_expr.BareName name)

let parse_type_list parse_type_expr' types =
  List.fold_right (fun t types ->
    types >>= fun types ->
    (parse_type_expr' t) >>= fun t ->
    return (t :: types)) types (Ok [])

let rec parse_type_expr = function
  | Form.Symbol tipe -> begin
      (parse_name_expr tipe) >>= fun tipe ->
      return (Type_expr.SimpleType tipe)
  end
  | Form.Vec types -> begin
   match parse_type_list parse_type_expr types with
   | Error e -> Error e
   | Ok [] -> invalid_type_error
   | Ok types -> Ok (Type_expr.FnType types)
  end
  | _ -> invalid_type_error

let parse_var_def = function
  | (Form.Symbol name :: tipe :: []) -> begin
      (parse_type_expr tipe) >>= fun tipe ->
      let name = Node.VarDef.Name.from_string name in
      return (name, tipe)
  end
  | _ -> Error (Cmpl_err.ParseError "invalid VAR form")

let parse_rec_fields f_parse fields =
  let fold_fn (name, tipe) fields =
    fields >>= fun fields ->
    (parse_var_def [name; tipe]) >>= fun (name, tipe) ->
    let field = Node.VarDef.from_parts name tipe in
    return (field :: fields) in
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

let is_const_literal = function
  | Form.Number _ | Form.String _ -> true
  | Form.List (Form.Symbol "fn" :: rest) -> true
  | _ -> false

let parse_def f_parse = function
  | Form.Symbol name :: expr :: [] when is_const_literal expr ->
      (f_parse expr) >>= fun expr ->
      let name = Node.Name.from_string name in
      return (Node.Def (name, expr))
  | Form.Symbol name :: expr :: [] ->
      Error (Cmpl_err.ParseError "DEF EXPR must evaluate to constant value")
  | _ -> Error (Cmpl_err.ParseError "invalid DEF form")

let parse_params params =
  let fold_fn (name, tipe) params =
    params >>= fun params ->
    (parse_var_def [name; tipe]) >>= fun (name, tipe) ->
    let param = Node.VarDef.from_parts name tipe in
    return (param :: params) in
  if (List.length params mod 2) = 0 then
    List.fold_right fold_fn (List.as_pairs params) (Ok [])
  else Error (Cmpl_err.ParseError "invalid VARS form")

let parse_header header =
  match List.rev header with
  | rtype :: Form.Vec raw_params :: [] -> begin
      (parse_params raw_params) >>= fun params ->
      (parse_type_expr rtype) >>= fun rtype ->
      return (params, rtype)
  end
  | _ -> Error (Cmpl_err.ParseError "invalid FN header")

let parse_fn f_parse = function
  | Form.Vec raw_header :: raw_body :: [] ->
      (parse_header raw_header) >>= fun (params, rtype) ->
      (f_parse raw_body) >>= fun body ->
      return (Node.Fn (params, rtype, body))
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
    (parse_type_expr raw_type) >>= fun t ->
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

let nested_error = Cmpl_err.ParseError "definitions must occur at toplevel"

let parse_op f_parse op (args: Form.t list) =
  if op = "fn" then parse_fn f_parse args
  else if op = "if" then parse_if f_parse args
  else if op = "let" then parse_let f_parse args
  else if op = "cast" then parse_cast f_parse args
  else if op = "def" || op = "defrec" then Error nested_error
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

let toplevel_error = Cmpl_err.ParseError "toplevel forms must be definitions"

let parse_toplevel = function
  | Form.List (Form.Symbol op :: args) -> begin
    if op = "defrec" then parse_rec parse_form args
    else if op = "def" then parse_def parse_form args
    else Error toplevel_error
  end
  | _ -> Error toplevel_error

let parse forms =
  let fold_fn form forms =
    forms >>= fun fs ->
    (parse_toplevel form) >>= fun f ->
    return (f :: fs) in
  List.fold_right fold_fn forms (Ok [])
