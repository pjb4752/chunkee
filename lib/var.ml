open Printf

type t = {
  name: Identifier.t;
  tipe: Type.t
}

let make name tipe = { name; tipe }

let name { name; _ } = name

let tipe { tipe; _ } = tipe

let to_string { name; _ } = sprintf "(var %s)" (Identifier.to_string name)

let inspect { name; tipe } = sprintf "Var(%s, %s)" (Identifier.inspect name) (Type.to_string tipe)
