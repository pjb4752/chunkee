open Printf
open Result

let ifndef_var modul var_name =
  if Module.var_exists modul var_name then
    let str_name = Module.Var.Name.to_string var_name in
    Error (Cmpl_err.NameError (sprintf "var %s already defined" str_name))
  else
    let var = Module.Var.make var_name in
    Ok (Module.add_var modul var)

let define_var modul = function
  | Node.Def (name, _) -> ifndef_var modul name
  | _ -> Ok (modul)

let define_vars modul nodes =
  let fold_fn node modul =
    modul >>= fun m ->
    (define_var m node) >>= fun m ->
    return m in
  List.fold_right fold_fn nodes (Ok modul)
