module Name = Id

module Var = struct
  module Name = Id

  type t = {
    name: Name.t
  }
end

type t = {
  name: Name.t;
  vars: Var.t list;
}

let make name = { name; vars = [] }

let find_var modul var_name =
  List.find_opt (fun (v: Var.t) -> v.name = var_name) modul.vars

let add_var modul var =
  { modul with vars = var :: modul.vars }
