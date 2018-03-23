open Printf

module NameError = struct
  type t = {
    message: string;
  }

  let message e = e.message
  let to_string e = "NameError: " ^ message e
end

let define_var modul var_name =
  if Module.var_exists modul var_name then
    let str_name = Module.Var.Name.to_string var_name in
    Error { NameError.message = sprintf "var %s already defined" str_name }
  else
    let var = Module.Var.make var_name in
    Ok (Module.add_var modul var)

let define_vars modul = function
  | Node.Def (name, _) -> define_var modul name
  | _ -> Ok (modul)
