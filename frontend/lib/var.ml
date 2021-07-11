open Printf

type t = {
  name: string;
  tipe: Type.t
}

let make name tipe = { name; tipe }

let name { name; _ } = name

let tipe { tipe; _ } = tipe

let inspect { name; tipe } = sprintf "Var(%s, %s)" name (Type.inspect tipe)
