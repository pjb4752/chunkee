open Printf
open Thwack.Result

type node_t = Resolve.Resolve.node_t

type t = Resolve.Resolve.node_t * Type.t

module Scope = Map.Make(String)

let is_compatible this that =
  this = that || this == Type.Any

let are_compatible this that =
  List.for_all2 is_compatible this that

let find_type t =
  match Type.from_node t with
  | Some t -> Ok t
  | None ->
    let t = Node.TypeDef.to_string t in
    Error (Cmpl_err.TypeError (sprintf "type %s not found" t))

let chk_local_name scopes name =
  match List.find_opt (Scope.mem name) scopes with
  | None -> assert false
  | Some s -> begin
      match Scope.find_opt name s with
      | None -> assert false
      | Some t -> Ok t
  end

let chk_module_name table qual_name var_name =
  match Symbol_table.find_module table qual_name with
  | None -> assert false
  | Some m -> begin
    match Module.find_var m var_name with
    | None -> assert false
    | Some v ->
        let (_, _, t) = Module.Var.to_tuple v in
        Ok t
  end

let chk_name table modul scopes = function
  | Name.Local n -> chk_local_name scopes n
  | Name.Module (qn, vn) -> chk_module_name table qn vn

let chk_def recur_fn modul scopes var expr =
  let (name, _) = Node.VarDef.to_tuple var in
  let var = match Module.find_var modul name with
  | Some v -> v
  | None -> assert false in
  let expr_type = (recur_fn scopes expr) >>= fun e -> return e
  and (_, _, t_exp) = Module.Var.to_tuple var in
  match expr_type with
  | Error e -> Error e
  | Ok t_act when is_compatible t_exp t_act -> Ok t_exp
  | _ -> Error (Cmpl_err.TypeError ("def-expr did not evaluate to given type"))

let process_params params =
  let fold_fn p ps =
    let (name, t) = Node.VarDef.to_tuple p in
    let name = Node.VarDef.Name.to_string name in
    ps >>= fun ps ->
    (find_type t) >>= fun t ->
    return ((name, t) :: ps) in
  match List.fold_right fold_fn params (Ok []) with
  | Error e -> Error e
  | Ok ps ->
    let scope = List.fold_right (fun (n, t) s -> Scope.add n t s) ps Scope.empty
    and types = List.map (fun (n, t) -> t) ps in
    Ok (types, scope)

(*TODO should return type be explicit instead of inferred*)
let chk_fn recur_fn scopes params body =
  (process_params params) >>= fun (t, s) ->
  (recur_fn (s :: scopes) body) >>= fun rt ->
  return (Type.Fn (t, rt))

let cmp_tst_expr = function
  | Type.Bool -> Ok Type.Bool
  | _ -> Error (Cmpl_err.TypeError ("if-test-expr must evaluate to boolean"))

let chk_if_tst recur_fn scopes tst =
  (recur_fn scopes tst) >>= fun t ->
  (cmp_tst_expr t) >>= fun r ->
  return r

let cmp_if_expr iff els =
  if iff = els then Ok iff
  else if iff = Type.Any || els = Type.Any then Ok Type.Any
  else Error (Cmpl_err.TypeError ("if-else-expr must evaluate to same type"))

let chk_if recur_fn scopes tst iff els =
  (chk_if_tst recur_fn scopes tst) >>= fun t ->
  (recur_fn scopes iff) >>= fun i ->
  (recur_fn scopes els) >>= fun e ->
  (cmp_if_expr i e) >>= fun r ->
  return r

let chk_binding recur_fn scopes binding =
  let (name, expr) = Node.Binding.to_tuple binding in
  match recur_fn scopes expr with
  | Error e -> Error e
  | Ok t -> begin
    let name = Node.Binding.Name.to_string name in
    let (scope: 'a Scope.t) = Scope.add name t Scope.empty in
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
  (chk_bindings scopes bindings) >>= fun s ->
  (recur_fn s body) >>= fun b ->
  return b

let cmp_fn_types f_type p_act =
  match f_type with
  | Type.Fn (p_exp, rt) when are_compatible p_exp p_act -> Ok rt
  | Type.Fn _ ->
      Error (Cmpl_err.TypeError "argument types do not match expected types")
  | _ -> Error (Cmpl_err.TypeError "cannot apply non-fn type")

let chk_apply recur_fn scopes fn args =
  let fold_fn arg types =
    types >>= fun ts ->
    (recur_fn scopes arg) >>= fun t ->
    return (t :: ts) in
  let types = List.fold_right fold_fn args (Ok []) in
  types >>= fun ts ->
  (recur_fn scopes fn) >>= fun f ->
  (cmp_fn_types f ts) >>= fun rt ->
  return rt

let chk_cast recur_fn scopes tdef expr =
  (find_type tdef) >>= fun t ->
  (recur_fn scopes expr) >>= fun e ->
  return t

let check_node table modul node =
  let rec check' scopes = function
    | Node.NumLit _ -> Ok Type.Num
    | Node.StrLit _ -> Ok Type.Str
    | Node.SymLit name -> chk_name table modul scopes name
    | Node.Def (var, expr) -> chk_def check' modul scopes var expr
    | Node.Fn (params, body) -> chk_fn check' scopes params body
    | Node.If (tst, iff, els) -> chk_if check' scopes tst iff els
    | Node.Let (bindings, body) -> chk_let check' scopes bindings body
    | Node.Apply (fn, args) -> chk_apply check' scopes fn args
    | Node.Cast (tdef, expr) -> chk_cast check' scopes tdef expr in
  check' [] node

let check table modul nodes =
  let fold_fn node typechecked =
    typechecked >>= fun tc ->
    (check_node table modul node) >>= fun t ->
    return ((node, t) :: tc) in
  List.fold_right fold_fn nodes (Ok [])
