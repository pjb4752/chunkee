open Printf

module Name = Id

module Var = struct
  module Name = Id

  type t = {
    name: Name.t
  }

  let make name = { name; }

  let to_string { name; } = sprintf "(var %s)" (Name.to_string name)
end

type t = {
  name: Name.t;
  vars: Var.t list;
}

let make name = { name; vars = [] }

let name { name; } = name

let find_var modul var_name =
  List.find_opt (fun (v: Var.t) -> v.name = var_name) modul.vars

let var_exists modul var_name =
  Option.is_some @@ find_var modul var_name

let add_var modul var =
  { modul with vars = var :: modul.vars }

let to_string { name; vars; } =
  let vars = String.concat " " (List.map Var.to_string vars) in
  sprintf "(module %s (%s))" (Name.to_string name) vars
