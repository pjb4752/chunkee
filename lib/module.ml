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

module Path = struct
  type t = Name.t list

  let from_list = function
    | [] -> assert false
    | parts -> parts

  let to_list path = path

  let to_string path =
    let names = List.map Name.to_string path in
    String.concat "." names
end

type t = {
  name: Name.t;
  path: Path.t;
  vars: Var.t list;
}

let make path name = { path; name; vars = [] }

let name { name; } = name

let full_path { path; name; } = List.append path [name;]

let path_list { path; } = path

let find_var modul var_name =
  List.find_opt (fun (v: Var.t) -> v.name = var_name) modul.vars

let var_exists modul var_name =
  Option.is_some @@ find_var modul var_name

let add_var modul var =
  { modul with vars = var :: modul.vars }

let to_string { path; name; vars; } =
  let vars = String.concat " " (List.map Var.to_string vars)
  and path = Path.to_string path
  and name = Name.to_string name in
  sprintf "(module %s.%s (%s))" path name vars
