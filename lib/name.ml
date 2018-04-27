open Printf

type t =
  | Local of string
  | Module of Module.Name.t

let to_string = function
  | Local s -> sprintf "(local %s)" s
  | Module n -> Module.Name.to_string n
