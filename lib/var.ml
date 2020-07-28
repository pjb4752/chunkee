open Printf

module Name = Id

type t = {
  name: Name.t;
  tipe: Type.t
}

let make name tipe = { name; tipe }

let name { name; _ } = name

let tipe { tipe; _ } = tipe

let to_string { name; _ } = sprintf "(var %s)" (Name.to_string name)

let inspect { name; tipe } = sprintf "Var(%s, %s)" (Name.to_string name) (Type.to_string tipe)
