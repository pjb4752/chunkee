open Printf

type t =
  | Literal of string
  | Local of string
  | Module of Module.Name.t

let to_string = function
  | Literal s -> sprintf "(literal %s)" s
  | Local s -> sprintf "(local %s)" s
  | Module n -> Module.Name.to_string n
