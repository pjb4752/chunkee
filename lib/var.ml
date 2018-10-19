open Printf

module Name = Id

type t = {
  name: Name.t;
  tipe: Type.t
}

let make name tipe = { name; tipe }

let name { name } = name

let tipe { tipe } = tipe

let to_string { name; } = sprintf "(var %s)" (Name.to_string name)
