open Printf
open Thwack.Result

type node_t = Resolve.Resolve.node_t

type n = Resolve.Resolve.node_t * Type.t
type s = (Module.t * Type.t, Cmpl_err.t) result
type t = (Module.t * n list, Cmpl_err.t) result

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
    | Some v -> begin
      match Var.tipe v with
      | Some tipe -> Ok tipe
      | _ -> assert false
    end
  end

let chk_name table modul scopes name =
  let tipe = match name with
  | Name.Var.Local n -> chk_local_name scopes n
  | Name.Var.Module (qn, vn) -> chk_module_name table qn vn in
  tipe >>= fun tipe -> return (modul, tipe)

let chk_rec recur_fn modul name fields =
  let m_name = Module.name modul in
  Ok (modul, Type.Rec (m_name, name))

let chk_def recur_fn modul scopes name expr =
  (recur_fn scopes expr) >>= fun (modul, tipe) ->
  return (Module.define_var modul name tipe, tipe)

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

(*TODO should return type be explicit instead of inferred*)
let chk_fn recur_fn scopes params body =
  (process_params params) >>= fun (ptype, scope) ->
  (recur_fn (scope :: scopes) body) >>= fun (modul, rtype) ->
  return (modul, Type.Fn (ptype, rtype))

let cmp_tst_expr = function
  | Type.Bool -> Ok Type.Bool
  | _ -> Error (Cmpl_err.TypeError ("if-test-expr must evaluate to boolean"))

let chk_if_tst recur_fn scopes tst =
  (recur_fn scopes tst) >>= fun (modul, ttype) ->
  (cmp_tst_expr ttype) >>= fun ttype ->
  return (modul, ttype)

let cmp_if_expr iff els =
  if iff = els then Ok iff
  else if iff = Type.Any || els = Type.Any then Ok Type.Any
  else Error (Cmpl_err.TypeError ("if-else-expr must evaluate to same type"))

let chk_if recur_fn scopes tst iff els =
  (chk_if_tst recur_fn scopes tst) >>= fun (modul, ttype) ->
  (recur_fn scopes iff) >>= fun (modul, itype) ->
  (recur_fn scopes els) >>= fun (modul, etype) ->
  (cmp_if_expr itype etype) >>= fun rtype ->
  return (modul, rtype)

let chk_binding recur_fn scopes binding =
  let (name, expr) = Node.Binding.to_tuple binding in
  match recur_fn scopes expr with
  | Error e -> Error e
  | Ok (_, tipe) -> begin
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
  (recur_fn scopes body) >>= fun (modul, rtype) ->
  return (modul, rtype)

let cmp_fn_types f_type p_act =
  match f_type with
  | Type.Fn (p_exp, rt) when are_compatible p_exp p_act -> Ok rt
  | Type.Fn _ ->
      Error (Cmpl_err.TypeError "argument types do not match expected types")
  | _ -> Error (Cmpl_err.TypeError "cannot apply non-fn type")

let chk_apply recur_fn scopes fn args =
  let fold_fn arg types =
    types >>= fun types ->
    (recur_fn scopes arg) >>= fun (_, tipe) ->
    return (tipe :: types) in
  let types = List.fold_right fold_fn args (Ok []) in
  types >>= fun etypes ->
  (recur_fn scopes fn) >>= fun (modul, atypes) ->
  (cmp_fn_types atypes etypes) >>= fun rtype ->
  return (modul, rtype)

let chk_cast recur_fn scopes tipe expr =
  (recur_fn scopes expr) >>= fun (modul, _) ->
  return (modul, tipe)

let check_node table modul node =
  let rec check' scopes = function
    | Node.NumLit _ -> Ok (modul, Type.Num)
    | Node.StrLit _ -> Ok (modul, Type.Str)
    | Node.SymLit name -> chk_name table modul scopes name
    | Node.Rec (name, fields) -> chk_rec check' modul name fields
    | Node.Def (name, expr) -> chk_def check' modul scopes name expr
    | Node.Fn (params, body) -> chk_fn check' scopes params body
    | Node.If (tst, iff, els) -> chk_if check' scopes tst iff els
    | Node.Let (bindings, body) -> chk_let check' scopes bindings body
    | Node.Apply (fn, args) -> chk_apply check' scopes fn args
    | Node.Cast (tipe, expr) -> chk_cast check' scopes tipe expr in
  check' [] node

let check table modul nodes =
  let fold_fn node accumulator =
    accumulator >>= fun (modul, node_types) ->
    (check_node table modul node) >>= fun (modul, tipe) ->
    return (modul, (node, tipe) :: node_types) in
  List.fold_right fold_fn nodes (Ok (modul, []))
