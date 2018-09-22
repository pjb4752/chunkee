open Printf

module Name = Id

type t = {
  name: Name.t;
  tipe: Type.t option
}

let declare name = { name; tipe = None }

let define name tipe = { name; tipe = Some tipe }

let name { name } = name

let tipe { tipe } = tipe

let to_string { name; } = sprintf "(var %s)" (Name.to_string name)
