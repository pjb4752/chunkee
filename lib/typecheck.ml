open Printf
open Thwack.Result

module Node = Ast.Resolved_node

type t = ((Node.t * Type.t) list, Cmpl_err.t) result

type u = (Type.t, Cmpl_err.t) result

module Scope = Map.Make(String)

let is_compatible expected actual =
  expected = actual || expected == Type.Any

let are_compatible expected actual =
  List.for_all2 is_compatible expected actual

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
  let tipe = match name with
  | Name.Var.Local name -> chk_local_name scopes name
  | Name.Var.Module (mod_name, var_name) ->
      chk_module_name table mod_name var_name in
  tipe >>= fun tipe -> return tipe

let process_params params =
  let fold_fn p ps =
    let (name, tipe) = Node.VarDef.to_tuple p in
    let name = Node.VarDef.Name.to_string name in
    ps >>= fun ps ->
    return ((name, tipe) :: ps) in
  match List.fold_right fold_fn params (Ok []) with
  | Error e -> Error e
  | Ok ps ->
    let scope = List.fold_right (fun (n, t) s -> Scope.add n t s) ps Scope.empty
    and types = List.map (fun (n, t) -> t) ps in
    Ok (types, scope)

let chk_fn recur_fn scopes params rtype body =
  let maybe_rtype =
    (process_params params) >>= fun (ptype, scope) ->
    (recur_fn (scope :: scopes) body) >>= fun rtype ->
    return rtype in
  match maybe_rtype with
  | Error e -> Error e
  | Ok actual_rtype when rtype = actual_rtype -> Ok actual_rtype
  | _ ->
      let message = "fn actual return type does not match " ^
                    "expected return type" in
      Error (Cmpl_err.TypeError message)

let cmp_tst_expr = function
  | Type.Bool -> Ok Type.Bool
  | _ -> Error (Cmpl_err.TypeError ("if-test-expr must evaluate to boolean"))

let chk_if_tst recur_fn scopes tst =
  (recur_fn scopes tst) >>= fun ttype ->
  (cmp_tst_expr ttype) >>= fun ttype ->
  return ttype

let cmp_if_expr iff els =
  if iff = els then Ok iff
  else if iff = Type.Any || els = Type.Any then Ok Type.Any
  else Error (Cmpl_err.TypeError ("if-else-expr must evaluate to same type"))

let chk_if recur_fn scopes tst iff els =
  (chk_if_tst recur_fn scopes tst) >>= fun ttype ->
  (recur_fn scopes iff) >>= fun itype ->
  (recur_fn scopes els) >>= fun etype ->
  (cmp_if_expr itype etype) >>= fun rtype ->
  return rtype

let chk_binding recur_fn scopes binding =
  let (name, expr) = Node.Binding.to_tuple binding in
  match recur_fn scopes expr with
  | Error e -> Error e
  | Ok tipe -> begin
    let name = Node.Binding.Name.to_string name in
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
  (chk_bindings scopes bindings) >>= fun scopes ->
  (recur_fn scopes body) >>= fun rtype ->
  return rtype

let cmp_fn_types f_type p_act =
  match f_type with
  | Type.Fn (p_exp, rt) -> begin
    let comparison = List.compare_lengths p_exp p_act in
    if comparison = 0 && (are_compatible p_exp p_act) then Ok rt
    else if comparison < 0 then
      Error (Cmpl_err.TypeError "too many arguments passed to function")
    else if comparison > 0 then
      Error (Cmpl_err.TypeError "too few arguments passed to fucntion")
    else
      Error (Cmpl_err.TypeError "argument types do not match expected types")
  end
  | _ -> Error (Cmpl_err.TypeError "cannot apply non-fn type")

let chk_apply recur_fn scopes fn args =
  let fold_fn arg types =
    types >>= fun types ->
    (recur_fn scopes arg) >>= fun tipe ->
    return (tipe :: types) in
  let types = List.fold_right fold_fn args (Ok []) in
  types >>= fun etypes ->
  (recur_fn scopes fn) >>= fun atypes ->
  (cmp_fn_types atypes etypes) >>= fun rtype ->
  return rtype

let compare_cons_type name rtype cons =
  match List.find_opt (fun c -> (fst c) = name) cons with
  | Some (_, tipe) when is_compatible tipe rtype -> Ok rtype
  | Some (_, tipe) ->
      let message = "record binding does not match expected type" in
      Error (Cmpl_err.TypeError message)
  | None -> assert false

let chk_cons recur_fn scopes tipe bindings =
  match tipe with
  | Type.Rec (_, _, cons) -> begin
    let fold_fn binding rtypes =
      rtypes >>= fun rtypes ->
      let (name, expr) = Node.Binding.to_tuple binding in
      (recur_fn scopes expr) >>= fun rtype ->
      (compare_cons_type name rtype cons) >>= fun rtype ->
      return (rtype :: rtypes) in
    let rtypes = List.fold_right fold_fn bindings (Ok []) in
    rtypes >>= fun rtypes -> return tipe
    end
  | _ -> assert false

let chk_record_type = function
  | Type.Rec (_, _, cons) -> Ok cons
  | _ -> Error (Cmpl_err.TypeError "first arg to 'get' must be record type")

let chk_record_field cons field =
  match List.find_opt (fun (name, _) -> name = field) cons with
  | Some (_, tipe) -> Ok (tipe)
  | None -> Error (Cmpl_err.NameError "record does not have that field")

let chk_get table scopes record field =
  match record with
  | Node.SymLit name -> begin
    (chk_name table scopes name) >>= fun rectype ->
    (chk_record_type rectype) >>= fun cons ->
    (chk_record_field cons field) >>= fun rtype ->
    return rtype
  end
  | _ -> assert false

let compare_set_type expected actual =
  if is_compatible expected actual then Ok actual
  else Error (Cmpl_err.TypeError "record set expr is not of expected type")

let chk_set recur_fn table scopes record field expr =
  match record with
  | Node.SymLit name -> begin
    (chk_name table scopes name) >>= fun rectype ->
    (chk_record_type rectype) >>= fun cons ->
    (chk_record_field cons field) >>= fun fieldtype ->
    (recur_fn scopes expr) >>= fun exprtype ->
    (compare_set_type exprtype fieldtype) >>= fun fieldtype ->
    return Type.Unit
  end
  | _ -> assert false

let chk_cast recur_fn scopes tipe expr =
  (recur_fn scopes expr) >>= fun _ ->
  return tipe

let check_node table node =
  let rec check_node' scopes = function
    | Node.NumLit n -> Ok Type.Num
    | Node.StrLit s -> Ok Type.Str
    | Node.SymLit name -> chk_name table scopes name
    | Node.Fn (params, rtype, body) ->
        chk_fn check_node' scopes params rtype body
    | Node.If (tst, iff, els) -> chk_if check_node' scopes tst iff els
    | Node.Let (bindings, body) -> chk_let check_node' scopes bindings body
    | Node.Apply (fn, args) -> chk_apply check_node' scopes fn args
    | Node.Cons (tipe, bindings) -> chk_cons check_node' scopes tipe bindings
    | Node.Get (record, field) -> chk_get table scopes record field
    | Node.Set (record, field, expr) ->
        chk_set check_node' table scopes record field expr
    | Node.Cast (tipe, expr) -> chk_cast check_node' scopes tipe expr
    | Node.Def _ -> assert false
    | Node.Rec _ -> assert false in
  check_node' [] node

(* TODO unify the logic here into one flow *)
let check_top_node table node =
  match node with
  | Node.Def (name, expr) -> begin
    (check_node table expr) >>= fun tipe ->
    return tipe
  end
  | Node.Rec (name, _) -> begin
    let modul = Symbol_table.current_module table in
    let mod_name = Module.name modul in
    match Symbol_table.module_type table mod_name name with
    | None -> assert false
    | Some tipe -> Ok tipe
  end
  | _ -> assert false

let check_top_nodes table nodes =
  let fold_fn node nodes =
    nodes >>= fun nodes ->
    (check_top_node table node) >>= fun tipe ->
    return ((node, tipe) :: nodes) in
  List.fold_right fold_fn nodes (Ok [])

let check table nodes =
  (check_top_nodes table nodes) >>= fun nodes ->
  return nodes
