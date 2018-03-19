open Printf

module Param = Id

type t =
  | NumLit of float
  | StrLit of string
  | SymLit of string
  | Def of (Module.Var.Name.t * t)
  | Fn of (Param.t list * t)

let rec to_string node =
  let string_of_def name expr =
    let name = Module.Var.Name.to_string name in
    sprintf "Def(%s, %s)" name (to_string expr) in
  let string_of_fn params body =
    let params = String.concat ", " (List.map Param.to_string params) in
    sprintf "Fn(Params(%s), %s)" params (to_string body) in
  match node with
  | NumLit n -> sprintf "NumLit(%.2f)" n
  | StrLit s -> sprintf "StrLit(%s)" s
  | SymLit s -> sprintf "SymLit(%s)" s
  | Def (name, expr) -> string_of_def name expr
  | Fn (params, body) -> string_of_fn params body
