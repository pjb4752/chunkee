module Param = Id

type t =
  | NumLit of float
  | StrLit of string
  | SymLit of string
  | Def of (Module.Var.Name.t * t)
  | Fn of (Param.t list * t)

val to_string: t -> string
