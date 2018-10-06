open Printf
open Thwack.Result

module Node = Ast.Parsed_node

type t = (Module.t, Cmpl_err.t) result

let declare_var modul name =
  let str_name = Node.Name.to_string name in
  let var_name = Var.Name.from_string str_name in
  if Module.var_exists modul var_name then
    let message = sprintf "var %s already declared" str_name in
    Error (Cmpl_err.NameError message)
  else
    Ok (Module.declare_var modul var_name)

let declare_rec modul type_name =
  if Module.type_exists modul type_name then
    let name = Type.Name.to_string type_name in
    let message = sprintf "type %s already declared" name in
    Error (Cmpl_err.NameError message)
  else
    Ok (Module.declare_record modul type_name)

let declare_toplevel modul = function
  | Node.Def (name, _) -> declare_var modul name
  | Node.Rec (name, _) -> declare_rec modul name
  | _ -> Ok modul

let declare_toplevels modul nodes =
  let fold_fn node modul =
    modul >>= fun modul ->
    (declare_toplevel modul node) >>= fun modul ->
    return modul in
  List.fold_right fold_fn nodes (Ok modul)
