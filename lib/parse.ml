open Lex
open Printf
open Thwack.Extensions
open Thwack.Extensions.Result
open Thwack.Extensions.Result.Syntax

module Node = Ast.Parsed_node

type t = (Node.t list, Cmpl_err.t) result

let build_prefix { Metadata.line_num; char_num } =
  sprintf "in expression at %d:%d" line_num char_num

let parse_qual_name name metadata =
  match String.split_on_char '/' name with
  | mod_name :: name :: [] -> begin
    let mod_parts = String.split_on_char '.' mod_name in
    let mod_parts = List.map (Mod_name.Segment.from_string) mod_parts in
    match List.rev mod_parts with
    | mod_name :: path_parts -> begin
      let mod_path = Mod_name.Path.from_list path_parts in
      let mod_name = Mod_name.make mod_path mod_name in
      Ok (Name_expr.QualName (mod_name, name))
    end
    | _ ->
        let prefix = build_prefix metadata in
        Error (Cmpl_err.parse_errors metadata prefix [
          "qualified names must contain a full path to a module, ";
          sprintf "but instead found: %s" name;
          "\n\tplease use the correct form foo/bar.baz instead of foo/baz"
      ])
  end
  | _ ->
      let prefix = build_prefix metadata in
      Error (Cmpl_err.parse_errors metadata prefix [
        "qualified names must contain both module tree and path information, ";
        sprintf "but instead found: %s" name;
        "\n\tplease use the correct form foo/bar.baz instead of bar.baz"
    ])

let parse_name_expr name meta =
  if String.contains name '.' then parse_qual_name name meta
  else Ok (Name_expr.BareName name)

let parse_type_list parse_type_expr' types =
  List.fold_right (fun t types ->
    let* types = types in
    let* t = parse_type_expr' t in
    return (t :: types)) types (Ok [])

let invalid_type_error raw metadata =
  let prefix = build_prefix metadata in
  Error (Cmpl_err.parse_errors metadata prefix [
    "type expressions must be of a valid form, ";
    sprintf "but instead found: %s" raw;
    "\n\tplease use a correct simple or aggregate type"
  ])

let rec parse_type_expr = function
  | Form.Symbol (tipe, metadata) -> begin
    let* tipe = parse_name_expr tipe metadata in
    return (Type_expr.SimpleType tipe)
  end
  | Form.Cons (tipe, metadata) -> begin
    let* tipe = parse_name_expr tipe metadata in
    return (Type_expr.SimpleType tipe)
  end
  | Form.Vec (types, metadata) -> begin
   match parse_type_list parse_type_expr types with
   | Error e -> Error e
   | Ok [] ->
       let prefix = build_prefix metadata in
       Error (Cmpl_err.parse_errors metadata prefix [
        "aggregate type expressions must contain 1 or more types, instead found an empty expression";
        "\n\tplease use the correct form [type1 type2 type3]"
     ])
   | Ok types -> Ok (Type_expr.FnType types)
  end
  | form -> begin
    let error_fn = invalid_type_error @@ Form.to_string form in
    match form with
    | Form.Number (_, metadata) -> error_fn metadata
    | Form.String (_, metadata) -> error_fn metadata
    | Form.List (_, metadata) -> error_fn metadata
    | _ -> assert false
  end

let parse_var_def = function
  | (Form.Symbol (name, _) :: tipe :: []) -> begin
    let* tipe = (parse_type_expr tipe) in
    let name = Identifier.from_string name in
    return (name, tipe)
  end
  | first :: last :: [] -> begin
    let metadata = Form.metadata first in
    let first = Form.to_string first in
    let last = Form.to_string last in
    let prefix = build_prefix metadata in
    Error (Cmpl_err.parse_errors metadata prefix [
      "variable definitions must be pairs of a name and a type, ";
      sprintf "but instead found: [%s %s]" first last;
      "\n\tplease use the correct form [name type]"
    ])
  end
  | _ -> assert false

let parse_rec_fields fields metadata =
  let fold_fn (name, tipe) fields =
    let* fields = fields in
    let* (name, tipe) = parse_var_def [name; tipe] in
    let field = Node.VarDef.from_parts name tipe in
    return (field :: fields) in
  if (List.length fields mod 2) = 0 then
    List.fold_right fold_fn (List.as_pairs fields) (Ok [])
  else
    let prefix = build_prefix metadata in
    let raw = String.concat " " @@ List.map Form.to_string fields in
    Error (Cmpl_err.parse_errors metadata prefix [
      "record fields must be pairs of a name and a type, ";
      sprintf "but instead found an odd number of forms: [%s]" raw;
      "\n\tplease use the correct form [name1 type1 name2 type2]"
    ])

let parse_rec metadata = function
  | Form.Cons (name, _) :: Form.Vec (fields, meta) :: [] ->
      let name = Identifier.from_string name in
      let fields = parse_rec_fields fields meta in
      let* fs = fields in
      return (Node.Rec (name, fs, metadata))
  | args -> begin
    let prefix = build_prefix metadata in
    let raw = String.concat " " @@ List.map Form.to_string args in
    Error (Cmpl_err.parse_errors metadata prefix [
      "record definitions must be a name and a vector of field definitions, ";
      sprintf "but instead found: (defrec %s)" raw;
      "\n\tplease use the correct form (defrec RecName [field1 type1])"
    ])
  end

let rec is_const_literal = function
  | Form.Number _ | Form.String _ -> true
  | Form.List (Form.Symbol ("fn", _) :: _, _) -> true
  | Form.List (Form.Cons _ :: Form.Vec (bindings, _) :: [], _) -> begin
    let forms = List.map snd @@ List.as_pairs bindings in
    List.for_all is_const_literal forms
  end
  | _ -> false

let parse_def f_parse metadata = function
  | Form.Symbol (name, _) :: expr :: [] when is_const_literal expr ->
      let* expr = f_parse expr in
      let name = Identifier.from_string name in
      return (Node.Def (name, expr, metadata))
  | Form.Symbol (name, _) :: expr :: [] -> begin
    let prefix = build_prefix metadata in
    let raw = sprintf "(def %s %s)" name @@ Form.to_string expr in
    Error (Cmpl_err.parse_errors metadata prefix [
      "top-level definitions must evaluate to a literal value or constant record, ";
      sprintf "but instead found: %s" raw;
      "\n\tplease use the correct form (def name <constant>)"
    ])
  end
  | args -> begin
    let prefix = build_prefix metadata in
    let args = String.concat " " @@ List.map Form.to_string args in
    Error (Cmpl_err.parse_errors metadata prefix [
      "top-level variable definitions must provide a name and an expr, ";
      sprintf "but instead found: (def %s)" args;
      "\n\tplease use the correct form (def name <constant>)"
    ])
  end

let parse_get metadata = function
  | Form.Symbol (record, meta) :: Form.Symbol (field, _) :: [] ->
      let* record = parse_name_expr record meta in
      let field = Identifier.from_string field in
      let symlit = Node.SymLit (record, meta) in
      return (Node.Get (symlit, field, metadata))
  | args -> begin
    let prefix = build_prefix metadata in
    let args = String.concat " " @@ List.map Form.to_string args in
    Error (Cmpl_err.parse_errors metadata prefix [
      "get expression must provide the record and field name, ";
      sprintf "but instead found: (get %s)" args;
      "\n\tplease use the correct form (get record field)"
    ])
  end

let parse_set f_parse metadata = function
  | Form.Symbol (record, meta) :: Form.Symbol (field, _) :: expr :: [] ->
      let* record = parse_name_expr record meta in
      let* expr = f_parse expr in
      let field = Identifier.from_string field in
      let symlit = Node.SymLit (record, meta) in
      return (Node.Set (symlit, field, expr, metadata))
  | args -> begin
    let prefix = build_prefix metadata in
    let args = String.concat " " @@ List.map Form.to_string args in
    Error (Cmpl_err.parse_errors metadata prefix [
      "set expression must provide record and field names, and expression, ";
      sprintf "but instead found: (get %s)" args;
      "\n\tplease use the correct form (set! record field expression)"
    ])
  end

let parse_params metadata params =
  let fold_fn (name, tipe) params =
    let* params = params in
    let* (name, tipe) = parse_var_def [name; tipe] in
    let param = Node.VarDef.from_parts name tipe in
    return (param :: params) in
  if (List.length params mod 2) = 0 then
    List.fold_right fold_fn (List.as_pairs params) (Ok [])
  else
    let prefix = build_prefix metadata in
    let raw = String.concat " " @@ List.map Form.to_string params in
    Error (Cmpl_err.parse_errors metadata prefix [
      "parameter lists must be pairs of a name and a type, ";
      sprintf "but instead found an odd number of forms: [%s]" raw;
      "\n\tplease use the correct form [name1 type1 name2 type2]"
    ])

let parse_header metadata header =
  match List.rev header with
  | rtype :: Form.Vec (raw_params, meta) :: [] -> begin
    let* params = parse_params meta raw_params in
    let* rtype = parse_type_expr rtype in
    return (params, rtype)
  end
  | header -> begin
    let prefix = build_prefix metadata in
    let args = String.concat " " @@ List.map Form.to_string header in
    Error (Cmpl_err.parse_errors metadata prefix [
      "function headers must be a vector containing a vector of parameters and the return type, ";
      sprintf "but instead found: [%s]" args;
      "\n\tplease use the correct form (fn [[name type] type] <body>)"
    ])
  end

let parse_fn f_parse metadata = function
  | Form.Vec (raw_header, meta) :: raw_body :: [] ->
      let* (params, rtype) = parse_header meta raw_header in
      let* body = f_parse raw_body in
      return (Node.Fn (params, rtype, body, metadata))
  | args -> begin
    let prefix = build_prefix metadata in
    let args = String.concat " " @@ List.map Form.to_string args in
    Error (Cmpl_err.parse_errors metadata prefix [
      "function forms must contain a function header and singular body expression, ";
      sprintf "but instead found: (fn %s)" args;
      "\n\tplease use the correct form (fn [[name type] type] <body>)"
    ])
  end

let parse_if f_parse metadata = function
  | raw_test :: raw_if :: raw_else :: [] ->
      let* t = f_parse raw_test in
      let* i = f_parse raw_if in
      let* e = f_parse raw_else in
      return (Node.If (t, i, e, metadata))
  | args -> begin
    let prefix = build_prefix metadata in
    let args = String.concat " " @@ List.map Form.to_string args in
    Error (Cmpl_err.parse_errors metadata prefix [
      "if forms must contain a test expression, an if expression and an else expression, ";
      sprintf "but instead found: (if %s)" args;
      "\n\tplease use the correct form (if <test> <then> <else>)"
    ])
  end

let parse_binding f_parse = function
  | (Form.Symbol (b, _), raw_expr) ->
      let name = Identifier.from_string b in
      let* e = f_parse raw_expr in
      return (Node.Binding.from_node name e)
  | (first, second) -> begin
    let metadata = Form.metadata first in
    let prefix = build_prefix metadata in
    let first = Form.to_string first in
    let second = Form.to_string second in
    Error (Cmpl_err.parse_errors metadata prefix [
      "a let binding must be a pair of a name and an expression, ";
      sprintf "but instead found: [%s %s]" first second;
      "\n\tplease use the correct form (let [name1 expr1] <body>)"
    ])
  end

let parse_bindings f_parse metadata bindings =
  let fold_fn binding prior =
    let* bs = prior in
    let* b = parse_binding f_parse binding in
    return (b :: bs)
  in
  if (List.length bindings mod 2) = 0 then
    List.fold_right fold_fn (List.as_pairs bindings) (Ok [])
  else
    let prefix = build_prefix metadata in
    let raw = String.concat " " @@ List.map Form.to_string bindings in
    Error (Cmpl_err.parse_errors metadata prefix [
      "let bindings must be pairs of a name and an expression, ";
      sprintf "but nstead found an odd number of forms: [%s]" raw;
      "\n\tplease use the correct form (let [name1 expr1 name2 expr2] <body>)"
    ])

let parse_let f_parse metadata = function
  | Form.Vec (bindings, meta) :: body :: [] ->
      let bindings = parse_bindings f_parse meta bindings in
      let* bi = bindings in
      let* b = f_parse body in
      return (Node.Let (bi, b, metadata))
  | args -> begin
    let prefix = build_prefix metadata in
    let args = String.concat " " @@ List.map Form.to_string args in
    Error (Cmpl_err.parse_errors metadata prefix [
      "let forms must contain a vector of variable bindings and a singular body expression, ";
      sprintf "but instead found: (let %s)" args;
      "\n\tplease use the correct form (let [name expr] <body>)"
    ])
  end

let parse_cast f_parse metadata = function
  | raw_type :: expr :: [] -> begin
    let* t = parse_type_expr raw_type in
    let* e = f_parse expr in
    return (Node.Cast (t, e, metadata))
  end
  | args -> begin
    let prefix = build_prefix metadata in
    let args = String.concat " " @@ List.map Form.to_string args in
    Error (Cmpl_err.parse_errors metadata prefix [
      "cast forms must contain a type definition and a singular expression, ";
      sprintf "but instead found: (cast %s)" args;
      "\n\tplease use the correct form (cast type expr)"
    ])
  end

let parse_args f_parse args =
  let fold_fn arg prior =
    let* args = prior in
    let* a = f_parse arg in
    return (a :: args) in
  List.fold_right fold_fn args (Ok [])

let parse_num_apply f_parse metadata num args =
  let* args = parse_args f_parse args in
  let numlit = Node.NumLit (num, metadata) in
  return (Node.Apply (numlit, args, metadata))

let parse_str_apply f_parse metadata str args =
  let* args = parse_args f_parse args in
  let strlit = Node.StrLit (str, metadata) in
  return (Node.Apply (strlit, args, metadata))

let parse_sym_apply f_parse metadata fn args =
  let* fn = parse_name_expr fn metadata in
  let* args = parse_args f_parse args in
  let symlit = Node.SymLit (fn, metadata) in
  return (Node.Apply (symlit, args, metadata))

let parse_fn_apply f_parse metadata fn args =
  let* fn = f_parse (Form.List (fn, metadata)) in
  let* args = parse_args f_parse args in
  return (Node.Apply (fn, args, metadata))

let nested_error metadata =
  let prefix = build_prefix metadata in
  Error (Cmpl_err.parse_errors metadata prefix [
    "def and defrecord may occur only as top level expressions, ";
    "instead found a definition in a nested scope"
  ])

let parse_op f_parse meta op (args: Form.t list) =
  if op = "get" then parse_get meta args
  else if op = "set!" then parse_set f_parse meta args
  else if op = "fn" then parse_fn f_parse meta args
  else if op = "if" then parse_if f_parse meta args
  else if op = "let" then parse_let f_parse meta args
  else if op = "cast" then parse_cast f_parse meta args
  else if op = "def" || op = "defrec" then nested_error meta
  else parse_sym_apply f_parse meta op args

let parse_cons f_parse metadata tipe = function
  | Form.Vec (bindings, meta) :: [] ->
      let* name_expr = parse_name_expr tipe meta in
      let type_expr = Type_expr.SimpleType name_expr in
      let* bindings = parse_bindings f_parse meta bindings in
      return (Node.Cons (type_expr, bindings, metadata))
  | args -> begin
    let prefix = build_prefix metadata in
    let raw = String.concat " " @@ List.map Form.to_string args in
    Error (Cmpl_err.parse_errors metadata prefix [
      "a constructor form must be a valid record definttion, ";
      sprintf "but instead found: (defrecord %s)" raw;
      "\n\tplease use the correct form (defrecord Name [field type ...])"
    ])
  end

let parse_symbol metadata symbol =
  let* symbol = parse_name_expr symbol metadata in
  return (Node.SymLit (symbol, metadata))

let parse_list f_parse metadata = function
  | Form.Number (n, meta) :: args -> parse_num_apply f_parse meta n args
  | Form.String (s, meta) :: args -> parse_str_apply f_parse meta s args
  | Form.Symbol (op, meta) :: args -> parse_op f_parse meta op args
  | Form.Cons (tipe, meta) :: args -> parse_cons f_parse meta tipe args
  | Form.List (expr, meta) :: args -> parse_fn_apply f_parse meta expr args
  | args -> begin
    let prefix = build_prefix metadata in
    let raw = String.concat " " @@ List.map Form.to_string args in
    Error (Cmpl_err.parse_errors metadata prefix [
      sprintf "failed to parse invalid form: %s" raw;
      "\n\tplease check your syntax"
    ])
  end

let rec parse_form = function
  | Form.Number (num, metadata) -> Ok (Node.NumLit (num, metadata))
  | Form.String (str, metadata) -> Ok (Node.StrLit (str, metadata))
  | Form.Symbol (sym, metadata) -> parse_symbol metadata sym
  | Form.List (lst, metadata) -> parse_list parse_form metadata lst
  | Form.Cons (_, metadata) -> begin
    let prefix = build_prefix metadata in
    Error (Cmpl_err.parse_errors metadata prefix [
      "found unexpected constructor form";
      "\n\tplease check your syntax"
    ])
  end
  | Form.Vec (_, metadata) -> begin
    let prefix = build_prefix metadata in
    Error (Cmpl_err.parse_errors metadata prefix [
      "found unexpected vector form";
      "\n\tplease check your syntax"
    ])
  end

let toplevel_error metadata raw =
  let prefix = build_prefix metadata in
  Error (Cmpl_err.parse_errors metadata prefix [
    sprintf "top-level forms must be either a def or defrecord, instead found %s" raw;
    "\n\tplease use the correct forms at the top-level"
  ])

let parse_toplevel = function
  | Form.List ((Form.Symbol (op, smeta) :: args), meta) -> begin
    if op = "defrec" then parse_rec meta args
    else if op = "def" then parse_def parse_form meta args
    else toplevel_error smeta op
  end
  | form -> toplevel_error (Form.metadata form) (Form.to_string form)

let parse forms =
  let fold_fn form forms =
    let* fs = forms in
    let* f = parse_toplevel form in
    return (f :: fs) in
  List.fold_right fold_fn forms (Ok [])

let inspect result =
  let nodes_to_string nodes =
    String.concat "; " @@ List.map Node.inspect nodes in
  Result.to_string result nodes_to_string Cmpl_err.to_string
