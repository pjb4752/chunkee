open Printf

module Name = Id

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

module Qual_name = struct
  type t = Path.t * Name.t

  let make path name = (path, name)

  let name (path, name) = name

  let path (path, name) = path

  let to_string (path, name) =
    sprintf "%s.%s" (Path.to_string path) (Name.to_string name)

  let to_list (path, name) =
    List.append (Path.to_list path) [name]
end

module Var = struct
  module Name = Id

  type t = {
    name: Name.t
  }

  let make name = { name; }

  let from_string name = make (Name.from_string name)

  let to_string { name; } = sprintf "(var %s)" (Name.to_string name)
end


type t = {
  name: Qual_name.t;
  vars: Var.t list;
}

let from_name name = { name; vars = [] }

let from_parts path name = from_name (Qual_name.make path name)

let short_name { name; } = Qual_name.name name

let qual_name { name; } = name

let path_list { name; } = Qual_name.path name

let qual_name_list { name; } = Qual_name.to_list name

let find_var modul var_name =
  List.find_opt (fun (v: Var.t) -> v.name = var_name) modul.vars

let var_exists modul var_name =
  Option.is_some @@ find_var modul var_name

let add_var modul var =
  { modul with vars = var :: modul.vars }

let make_var modul name =
  add_var modul (Var.from_string name)

let to_string { name; vars; } =
  let vars = String.concat " " (List.map Var.to_string vars)
  and name = Qual_name.to_string name in
  sprintf "(module %s (%s))" name vars
