type t =
  | NumLit of float
  | StrLit of string
  | SymLit of string
  | Def of (Module.Var.Name.t * t)

val to_string: t -> string
