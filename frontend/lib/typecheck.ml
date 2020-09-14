open Printf
open Common.Extensions
open Common.Extensions.Result
open Common.Extensions.Result.Syntax

module Node = Ast.Resolved_node

type t = (Type.t, Cmpl_err.t) result

module Scope = Map.Make(String)

let build_prefix message { Metadata.line_num; char_num } =
  sprintf "in %s at %d:%d" message line_num char_num

let is_compatible expected actual =
  expected = actual || expected == Type.Any

let chk_local_name scopes name =
  match List.find_opt (Scope.mem name) scopes with
  | None -> assert false
  | Some s -> begin
      match Scope.find_opt name s with
      | None -> assert false
      | Some t -> Ok t
  end

let chk_module_name table mod_name var_name =
  match Symbol_table.module_vartype table mod_name var_name with
  | None -> assert false
  | Some tipe -> Ok tipe

let chk_name table scopes name =
  match name with
  | Name.Var.Local name -> chk_local_name scopes name
  | Name.Var.Module (mod_name, var_name) -> chk_module_name table mod_name var_name

let process_params params =
  let fold_fn p ps =
    let (name, tipe) = Node.VarDef.to_tuple p in
    let name = Identifier.inspect name in
    let* ps = ps in
    return ((name, tipe) :: ps) in
  match List.fold_right fold_fn params (Ok []) with
  | Error e -> Error e
  | Ok ps ->
    let scope = List.fold_right (fun (n, t) s -> Scope.add n t s) ps Scope.empty
    and types = List.map (fun (_, t) -> t) ps in
    Ok (types, scope)

let chk_fn recur_fn scopes params rtype body metadata =
  let maybe_rtype =
    let* (_, scope) = process_params params in
    let* rtype = recur_fn (scope ::scopes) body in
    return rtype in
  match maybe_rtype with
  | Error e -> Error e
  | Ok actual_rtype when rtype = actual_rtype -> Ok actual_rtype
  | Ok actual_rtype ->
      let prefix = build_prefix "function definition" metadata in
      Error (Cmpl_err.type_errors metadata prefix [
        sprintf "expected return type is %s, " @@ Type.inspect rtype;
        sprintf "but actual return type found is %s" @@ Type.inspect actual_rtype
      ])

let cmp_tst_expr test_type metadata =
  match test_type with
  | Type.Bool -> Ok Type.Bool
  | tipe ->
      let prefix = build_prefix "if test-expr" metadata in
      Error (Cmpl_err.type_errors metadata prefix [
        "if test-exprs must evaulate to a boolean value, ";
        sprintf "instead received type of %s" @@ Type.inspect tipe
      ])

let chk_if_tst recur_fn scopes tst metadata =
  let* ttype = recur_fn scopes tst in
  let* ttype = cmp_tst_expr ttype metadata in
  return ttype

let cmp_if_expr iff els metadata =
  if iff = els then Ok iff
  else if iff = Type.Any || els = Type.Any then Ok Type.Any
  else
    let prefix = build_prefix "if expr" metadata in
    Error (Cmpl_err.type_errors metadata prefix [
      sprintf "result of if-expr was %s, " @@ Type.inspect iff;
      sprintf "which is incompatible with else-expr result type %s" @@ Type.inspect els
  ])

let chk_if recur_fn scopes tst iff els metadata =
  let* _ = chk_if_tst recur_fn scopes tst metadata in
  let* itype = recur_fn scopes iff in
  let* etype = recur_fn scopes els in
  let* rtype = cmp_if_expr itype etype metadata in
  return rtype

let chk_binding recur_fn scopes binding =
  let (name, expr) = Node.Binding.to_tuple binding in
  match recur_fn scopes expr with
  | Error e -> Error e
  | Ok tipe -> begin
    let name = Identifier.inspect name in
    let (scope: 'a Scope.t) = Scope.add name tipe Scope.empty in
    Ok (scope :: scopes)
  end

let chk_let recur_fn scopes bindings body =
  let rec chk_bindings scopes = function
    | [] -> Ok scopes
    | b :: bs -> begin
      match chk_binding recur_fn scopes b with
      | Error e -> Error e
      | Ok s -> chk_bindings s bs
    end in
  let* scopes = chk_bindings scopes bindings in
  let* rtype = recur_fn scopes body in
  return rtype

let chk_param_types expected_types actual_types return_type metadata =
  let chk_param_type index (expected, actual) =
    if is_compatible expected actual then None
    else
      let param_number = index + 1
      and expected_type = Type.inspect expected
      and actual_type = Type.inspect actual in
      Some [
        sprintf "function expected argument %d to be of type %s, " param_number expected_type;
        sprintf "instead received actual argument of type %s\n\t" actual_type
      ]
  in
  let combine_results errors = function
    | None -> errors
    | Some error -> errors @ error
  in
  let results = List.mapi chk_param_type @@ List.zip expected_types actual_types in
  let errors = List.fold_left combine_results [] results in
  if List.is_empty errors then Ok return_type
  else
    let prefix = build_prefix "function application" metadata in
    Error (Cmpl_err.type_errors metadata prefix errors)

let cmp_fn_types f_type p_act metadata =
  match f_type with
  | Type.Function (p_exp, rt) -> begin
    if List.compare_lengths p_exp p_act = 0 then
      chk_param_types p_exp p_act rt metadata
    else
      let prefix = build_prefix "function application" metadata in
      Error (Cmpl_err.type_errors metadata prefix [
        sprintf "function expected %d arguments, " @@ List.length p_exp;
        sprintf "but instead received %d" @@ List.length p_act
      ])
  end
  | tipe ->
      let prefix = build_prefix "expression" metadata in
      Error (Cmpl_err.type_errors metadata prefix [
        sprintf "attempt to apply non-function type of %s" @@ Type.inspect tipe
      ])

let chk_apply recur_fn scopes fn args metadata =
  let fold_fn arg types =
    let* types = types in
    let* tipe = recur_fn scopes arg in
    return (tipe :: types) in
  let types = List.fold_right fold_fn args (Ok []) in
  let* etypes = types in
  let* atypes = recur_fn scopes fn in
  let* rtype = cmp_fn_types atypes etypes metadata in
  return rtype

(*TODO handle this more like function arguments*)
let compare_field_types name rtype fields metadata =
  match List.find_opt (fun c -> (fst c) = name) fields with
  | Some (_, tipe) when is_compatible tipe rtype -> Ok rtype
  | Some (_, tipe) ->
      let prefix = build_prefix "record constructor" metadata in
      Error (Cmpl_err.type_errors metadata prefix [
        sprintf "constructor expected type of %s, " @@ Type.inspect tipe;
        sprintf "but instead received type of %s" @@ Type.inspect rtype
  ])
  | None -> assert false

let chk_cons recur_fn scopes tipe bindings metadata =
  match tipe with
  | Type.Record (fields) -> begin
    let fold_fn binding rtypes =
      let* rtypes = rtypes in
      let (name, expr) = Node.Binding.to_tuple binding in
      let* rtype = recur_fn scopes expr in
      let* rtype = compare_field_types name rtype fields metadata in
      return (rtype :: rtypes) in
    let rtypes = List.fold_right fold_fn bindings (Ok []) in
    let* _ = rtypes in return tipe
    end
  | _ -> assert false

let chk_record_type rectype metadata =
  match rectype with
  | Type.Record (fields) -> Ok fields
  | tipe ->
      let prefix = build_prefix "record.get operation" metadata in
      Error (Cmpl_err.type_errors metadata prefix [
        "first arg to 'get' builtin must be record type, ";
        sprintf "instead received %s" @@ Type.inspect tipe
  ])

let chk_record_field cons field metadata =
  match List.find_opt (fun (name, _) -> name = field) cons with
  | Some (_, tipe) -> Ok (tipe)
  | None ->
      let prefix = build_prefix "record.get operation" metadata in
      Error (Cmpl_err.name_errors metadata prefix [
        sprintf "record does not have field %s" @@ Identifier.inspect field
      ])

let chk_get table scopes record field metadata =
  match record with
  | Node.Symbol (name, _) -> begin
    let* rectype = chk_name table scopes name in
    let* cons = chk_record_type rectype metadata in
    let* rtype = chk_record_field cons field metadata in
    return rtype
  end
  | _ -> assert false

let compare_set_type actual expected metadata =
  if is_compatible expected actual then Ok actual
  else
    let prefix = build_prefix "record.set operation" metadata in
    Error (Cmpl_err.type_errors metadata prefix [
      sprintf "builtin 'set' expected type of %s, " @@ Type.inspect expected;
      sprintf "but instead received type of %s" @@ Type.inspect actual
    ])

let chk_set recur_fn table scopes record field expr metadata =
  match record with
  | Node.Symbol (name, _) -> begin
    let* rectype = chk_name table scopes name in
    let* cons = chk_record_type rectype metadata in
    let* fieldtype = chk_record_field cons field metadata in
    let* exprtype = recur_fn scopes expr in
    let* _ = compare_set_type exprtype fieldtype metadata in
    return Type.Unit
  end
  | _ -> assert false

let chk_cast recur_fn scopes tipe expr =
  let* _ = recur_fn scopes expr in
  return tipe

let check_node table node =
  let rec check_node' scopes = function
    | Node.NumLit (_, _) -> Ok Type.Number
    | Node.StrLit (_, _) -> Ok Type.String
    | Node.Symbol (name, _) -> chk_name table scopes name
    | Node.Fn (params, rtype, body, meta) -> chk_fn check_node' scopes params rtype body meta
    | Node.If (tst, iff, els, meta) -> chk_if check_node' scopes tst iff els meta
    | Node.Let (bindings, body, _) -> chk_let check_node' scopes bindings body
    | Node.Apply (fn, args, meta) -> chk_apply check_node' scopes fn args meta
    | Node.Cons (tipe, bindings, meta) -> chk_cons check_node' scopes tipe bindings meta
    | Node.Get (record, field, meta) -> chk_get table scopes record field meta
    | Node.Set (record, field, expr, meta) -> chk_set check_node' table scopes record field expr meta
    | Node.Cast (tipe, expr, _) -> chk_cast check_node' scopes tipe expr
    | Node.Type _ -> assert false
    | Node.Def _ -> assert false
    | Node.Rec _ -> assert false in
  check_node' [] node
