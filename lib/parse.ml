open Lex
open Printf
open Thwack.Result
open Thwack.Extensions

module Node = Ast.Parsed_node

let parse_qual_name name metadata =
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
    | _ -> Error (Cmpl_err.parse_errors metadata [
        "\tQualified names must contain a full path to a module";
        "\tInstead found: " ^ name;
        "\tPlease use the correct form foo/bar.baz instead of foo/baz"
      ])
  end
  | _ -> Error (Cmpl_err.parse_errors metadata [
      "\tQualified names must contain both module tree and path information";
      "\tInstead found: " ^ name;
      "\tPlease use the correct form foo/bar.baz instead of bar.baz"
    ])

let parse_name_expr name meta =
  if String.contains name '.' then parse_qual_name name meta
  else Ok (Name_expr.BareName name)

let parse_type_list parse_type_expr' types =
  List.fold_right (fun t types ->
    types >>= fun types ->
    (parse_type_expr' t) >>= fun t ->
    return (t :: types)) types (Ok [])

let invalid_type_error raw metadata =
  Error (Cmpl_err.parse_errors metadata [
    "\tType expressions must be of a valid form";
    "\tinstead found: " ^ raw;
    "\tPlease use a correct simple or aggregate type"
  ])

let rec parse_type_expr = function
  | Form.Symbol (tipe, meta) -> begin
      (parse_name_expr tipe meta) >>= fun tipe ->
      return (Type_expr.SimpleType tipe)
  end
  | Form.Cons (tipe, meta) -> begin
      (parse_name_expr tipe meta) >>= fun tipe ->
      return (Type_expr.SimpleType tipe)
  end
  | Form.Vec (types, meta) -> begin
   match parse_type_list parse_type_expr types with
   | Error e -> Error e
   | Ok [] -> Error (Cmpl_err.parse_errors meta [
        "\tAggregate type expressions must contain 1 or more types";
        "\tInstead found an empty expression";
        "\tPlease use the correct form [type1 type2 type3]"
     ])
   | Ok types -> Ok (Type_expr.FnType types)
  end
  | form -> begin
    let error_fn = invalid_type_error @@ Form.to_string form in
    match form with
    | Form.Number (_, meta) -> error_fn meta
    | Form.String (_, meta) -> error_fn meta
    | Form.List (_, meta) -> error_fn meta
    | _ -> assert false
  end

let parse_var_def = function
  | (Form.Symbol (name, _) :: tipe :: []) -> begin
      (parse_type_expr tipe) >>= fun tipe ->
      let name = Node.VarDef.Name.from_string name in
      return (name, tipe)
  end
  | first :: last :: [] -> begin
    let metadata = Form.metadata first in
    let first = Form.to_string first in
    let last = Form.to_string last in
    Error (Cmpl_err.parse_errors metadata [
      "\tVariable definitions must be pairs of a name and a type";
      "\tInstead found: " ^ (sprintf "[%s %s]" first last);
      "\tPlease use the correct form [name type]"
    ])
  end
  | _ -> assert false

let parse_rec_fields fields metadata =
  let fold_fn (name, tipe) fields =
    fields >>= fun fields ->
    (parse_var_def [name; tipe]) >>= fun (name, tipe) ->
    let field = Node.VarDef.from_parts name tipe in
    return (field :: fields) in
  if (List.length fields mod 2) = 0 then
    List.fold_right fold_fn (List.as_pairs fields) (Ok [])
  else
    let raw = String.concat " " @@ List.map Form.to_string fields in
    Error (Cmpl_err.parse_errors metadata [
      "\tRecord fields must be pairs of a name and a type";
      "\tInstead found an odd number of forms: " ^ (sprintf "[%s]" raw);
      "\tPlease use the correct form [name1 type1 name2 type2]"
    ])

let parse_rec metadata = function
  | Form.Cons (name, _) :: Form.Vec (fields, meta) :: [] ->
      let name = Node.Name.from_string name in
      let fields = parse_rec_fields fields meta in
      fields >>= fun fs ->
      return (Node.Rec (name, fs, metadata))
  | args -> begin
    let raw = String.concat " " @@ List.map Form.to_string args in
    Error (Cmpl_err.parse_errors metadata [
      "\tRecord definitions must be a name and a vector of field definitions";
      "\tInstead found: " ^ (sprintf "(defrec %s)" raw);
      "\tPlease use the correct form (defrec RecName [field1 type1])"
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
      (f_parse expr) >>= fun expr ->
      let name = Node.Name.from_string name in
      return (Node.Def (name, expr, metadata))
  | Form.Symbol (name, _) :: expr :: [] -> begin
    let raw = sprintf "(def %s %s)" name @@ Form.to_string expr in
    Error (Cmpl_err.parse_errors metadata [
      "\tTop-level definitions must evaluate to a constant value, which is:";
      "\t\taliteral number, string, function or constant record";
      "\tInstead found: " ^ raw;
      "\tPlease use the correct form (def name <constant>)"
    ])
  end
  | args -> begin
    let args = String.concat " " @@ List.map Form.to_string args in
    Error (Cmpl_err.parse_errors metadata [
      "\tTop-level variable definitions must provide a name and an expr";
      "\tInstead found: " ^ (sprintf "(def %s)" args);
      "\tPlease use the correct form (def name <constant>)"
    ])
  end

let parse_get metadata = function
  | Form.Symbol (record, meta) :: Form.Symbol (field, _) :: [] ->
      (parse_name_expr record meta) >>= fun record ->
      let field = Node.Name.from_string field in
      let symlit = Node.SymLit (record, meta) in
      return (Node.Get (symlit, field, metadata))
  | args -> begin
    let args = String.concat " " @@ List.map Form.to_string args in
    Error (Cmpl_err.parse_errors metadata [
      "\tGet expression must provide the record and field name";
      "\tInstead found: " ^ (sprintf "(get %s)" args);
      "\tPlease use the correct form (get record field)"
    ])
  end

let parse_set f_parse metadata = function
  | Form.Symbol (record, meta) :: Form.Symbol (field, _) :: expr :: [] ->
      (parse_name_expr record meta) >>= fun record ->
      (f_parse expr) >>= fun expr ->
      let field = Node.Name.from_string field in
      let symlit = Node.SymLit (record, meta) in
      return (Node.Set (symlit, field, expr, metadata))
  | args -> begin
    let args = String.concat " " @@ List.map Form.to_string args in
    Error (Cmpl_err.parse_errors metadata [
      "\tSet expression must provide record and field names, and expression";
      "\tInstead found: " ^ (sprintf "(get %s)" args);
      "\tPlease use the correct form (set! record field expression)"
    ])
  end

let parse_params metadata params =
  let fold_fn (name, tipe) params =
    params >>= fun params ->
    (parse_var_def [name; tipe]) >>= fun (name, tipe) ->
    let param = Node.VarDef.from_parts name tipe in
    return (param :: params) in
  if (List.length params mod 2) = 0 then
    List.fold_right fold_fn (List.as_pairs params) (Ok [])
  else
    let raw = String.concat " " @@ List.map Form.to_string params in
    Error (Cmpl_err.parse_errors metadata [
      "\tParameter lists must be pairs of a name and a type";
      "\tInstead found an odd number of forms: " ^ (sprintf "[%s]" raw);
      "\tPlease use the correct form [name1 type1 name2 type2]"
    ])

let parse_header metadata header =
  match List.rev header with
  | rtype :: Form.Vec (raw_params, meta) :: [] -> begin
      (parse_params meta raw_params) >>= fun params ->
      (parse_type_expr rtype) >>= fun rtype ->
      return (params, rtype)
  end
  | header -> begin
    let args = String.concat " " @@ List.map Form.to_string header in
    Error (Cmpl_err.parse_errors metadata [
      "\tFunction headers must be a vector of two, where the first item in";
      "\tin the vector is a vector of parameters,";
      "\tand the second is the return type";
      "\tInstead found: " ^ (sprintf "[%s]" args);
      "Please use the correct form (fn [[name type] type] <body>)"
    ])
  end

let parse_fn f_parse metadata = function
  | Form.Vec (raw_header, meta) :: raw_body :: [] ->
      (parse_header meta raw_header) >>= fun (params, rtype) ->
      (f_parse raw_body) >>= fun body ->
      return (Node.Fn (params, rtype, body, metadata))
  | args -> begin
    let args = String.concat " " @@ List.map Form.to_string args in
    Error (Cmpl_err.parse_errors metadata [
      "\tFunction forms must receive two arguments:";
      "\t\t1. a header containing the parameter definitions and return type";
      "\t\t2. a singular body expression";
      "\tInstead found: " ^ (sprintf "(fn %s)" args);
      "Please use the correct form (fn [[name type] type] <body>)"
    ])
  end

let parse_if f_parse metadata = function
  | raw_test :: raw_if :: raw_else :: [] ->
      (f_parse raw_test) >>= fun t ->
      (f_parse raw_if) >>= fun i ->
      (f_parse raw_else) >>= fun e ->
      return (Node.If (t, i, e, metadata))
  | args -> begin
    let args = String.concat " " @@ List.map Form.to_string args in
    Error (Cmpl_err.parse_errors metadata [
      "\tIf forms must recieve 3 arguments:";
      "\t\t1. a singluar test expression";
      "\t\t2. a singluar then expression (if test evaluates true)";
      "\t\t3. a singluar else expression (if test evalutates false)";
      "\tInstead found: " ^ (sprintf "(if %s)" args);
      "\tPlease use the correct form (if <test> <then> <else>)"
    ])
  end

let parse_binding f_parse = function
  | (Form.Symbol (b, _), raw_expr) ->
      let name = Node.Binding.Name.from_string b in
      (f_parse raw_expr) >>= fun e ->
      return (Node.Binding.from_node name e)
  | (first, second) -> begin
    let metadata = Form.metadata first in
    let first = Form.to_string first in
    let second = Form.to_string second in
    Error (Cmpl_err.parse_errors metadata [
      "\tA let binding must be a pair of a name and an expression";
      "\tInstead found: " ^ (sprintf "[%s %s]" first second);
      "\tPlease use the correct form (let [name1 expr1] <body>)"
    ])
  end

let parse_bindings f_parse metadata bindings =
  let fold_fn binding prior =
    prior >>= fun bs ->
    (parse_binding f_parse binding) >>= fun b ->
    return (b :: bs)
  in
  if (List.length bindings mod 2) = 0 then
    List.fold_right fold_fn (List.as_pairs bindings) (Ok [])
  else
    let raw = String.concat " " @@ List.map Form.to_string bindings in
    Error (Cmpl_err.parse_errors metadata [
      "\tLet bindings must be pairs of a name and an expression";
      "\tInstead found an odd number of forms: " ^ (sprintf "[%s]" raw);
      "\tPlease use the correct form (let [name1 expr1 name2 expr2] <body>)"
    ])

let parse_let f_parse metadata = function
  | Form.Vec (bindings, meta) :: body :: [] ->
      let bindings = parse_bindings f_parse meta bindings in
      bindings >>= fun bi ->
      (f_parse body) >>= fun b ->
      return (Node.Let (bi, b, metadata))
  | args -> begin
    let args = String.concat " " @@ List.map Form.to_string args in
    Error (Cmpl_err.parse_errors metadata [
      "\tLet forms must recieve 2 arguments:";
      "\t\t1. a vector of variable binding pairs";
      "\t\t2. a singluar body expression";
      "\tInstead found: " ^ (sprintf "(let %s)" args);
      "Please use the correct form (let [name expr] <body>)"
    ])
  end

let parse_cast f_parse metadata = function
  | raw_type :: expr :: [] -> begin
    (parse_type_expr raw_type) >>= fun t ->
    (f_parse expr) >>= fun e ->
    return (Node.Cast (t, e, metadata))
  end
  | args -> begin
    let args = String.concat " " @@ List.map Form.to_string args in
    Error (Cmpl_err.parse_errors metadata [
      "\tCast forms must recieve 2 arguments:";
      "\t\t1. a type definition the expression will be cast to";
      "\t\t2. a singluar expression";
      "\tInstead found: " ^ (sprintf "(let %s)" args);
      "Please use the correct form (cast type expr)"
    ])
  end

let parse_args f_parse args =
  let fold_fn arg prior =
    prior >>= fun args ->
    (f_parse arg) >>= fun a ->
    return (a :: args) in
  List.fold_right fold_fn args (Ok [])

let parse_num_apply f_parse metadata num args =
  (parse_args f_parse args) >>= fun args ->
  let numlit = Node.NumLit (num, metadata) in
  return (Node.Apply (numlit, args, metadata))

let parse_str_apply f_parse metadata str args =
  (parse_args f_parse args) >>= fun args ->
  let strlit = Node.StrLit (str, metadata) in
  return (Node.Apply (strlit, args, metadata))

let parse_sym_apply f_parse metadata fn args =
  (parse_name_expr fn metadata) >>= fun fn ->
  (parse_args f_parse args) >>= fun args ->
  let symlit = Node.SymLit (fn, metadata) in
  return (Node.Apply (symlit, args, metadata))

let parse_fn_apply f_parse metadata fn args =
  (f_parse (Form.List (fn, metadata))) >>= fun fn ->
  (parse_args f_parse args) >>= fun args ->
  return (Node.Apply (fn, args, metadata))

let nested_error metadata =
  Error (Cmpl_err.parse_errors metadata [
    "\tThe following definition forms can only occur at the top-level";
    "\t\t1. (def ...)";
    "\t\t2. (defrecord ...)";
    "\tInstead found a definition in a nested scope";
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
      (parse_name_expr tipe meta) >>= fun name_expr ->
      let type_expr = Type_expr.SimpleType name_expr in
      (parse_bindings f_parse meta bindings) >>= fun bindings ->
      return (Node.Cons (type_expr, bindings, metadata))
  | args -> begin
    let raw = String.concat " " @@ List.map Form.to_string args in
    Error (Cmpl_err.parse_errors metadata [
      "\tA constructor form must be a valid record definttion";
      "\tInstead found: " ^ (sprintf "(derecord %s)" raw);
      "\tPlease use the correct form (defrecord Name [field type ...])"
    ])
  end

let parse_symbol metadata symbol =
  (parse_name_expr symbol metadata) >>= fun symbol ->
  return (Node.SymLit (symbol, metadata))

let parse_list f_parse metadata = function
  | Form.Number (n, meta) :: args -> parse_num_apply f_parse meta n args
  | Form.String (s, meta) :: args -> parse_str_apply f_parse meta s args
  | Form.Symbol (op, meta) :: args -> parse_op f_parse meta op args
  | Form.Cons (tipe, meta) :: args -> parse_cons f_parse meta tipe args
  | Form.List (expr, meta) :: args -> parse_fn_apply f_parse meta expr args
  | args -> begin
    let raw = String.concat " " @@ List.map Form.to_string args in
    Error (Cmpl_err.parse_errors metadata [
      "\tFailed to parse valid form";
      "\tFound: " ^ (sprintf "%s" raw);
      "\tPlease check your syntax"
    ])
  end

let rec parse_form = function
  | Form.Number (n, meta) -> Ok (Node.NumLit (n, meta))
  | Form.String (s, meta) -> Ok (Node.StrLit (s, meta))
  | Form.Symbol (s, meta) -> parse_symbol meta s
  | Form.List (l, meta) -> parse_list parse_form meta l
  | Form.Cons (_, meta) -> begin
    Error (Cmpl_err.parse_errors meta [
      "\tFound unexpected Constructor form";
      "\tPlease check your syntax"
    ])
  end
  | Form.Vec (_, meta) -> begin
    Error (Cmpl_err.parse_errors meta [
      "\tFound unexpected Vector form";
      "\tPlease check your syntax"
    ])
  end

let toplevel_error metadata raw =
  Error (Cmpl_err.parse_errors metadata [
    "\tTop-level forms must be one of:";
    "\t\t1. (def ...)";
    "\t\t1. (defrecord ...)";
    "\tInstead found: " ^ raw;
    "\tPlease use the correct forms at the top-level"
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
    forms >>= fun fs ->
    (parse_toplevel form) >>= fun f ->
    return (f :: fs) in
  List.fold_right fold_fn forms (Ok [])
