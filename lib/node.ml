open Printf

module Param = Id

type t =
  | NumLit of float
  | StrLit of string
  | SymLit of string
  | Def of (Module.Var.Name.t * t)
  | Fn of (Param.t list * t)

let rec to_string node =
  let string_of_def n e  = Module.Var.Name.to_string n ^ ", " ^ to_string e in
  let string_of_fn params body =
    let params = String.concat ", " (List.map Param.to_string params) in
    sprintf "Fn(Params(%s), %s)" params (to_string body) in
  match node with
  | NumLit n -> "NumLit(" ^ string_of_float n ^ ")"
  | StrLit s -> "StrLit(" ^ s ^ ")"
  | SymLit s -> "SymLit(" ^ s ^ ")"
  | Def (n, e) -> "Def(" ^ string_of_def n e ^ ")"
  | Fn (params, body) -> string_of_fn params body
