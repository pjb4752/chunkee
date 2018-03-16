type t =
  | NumLit of float
  | StrLit of string
  | SymLit of string
  | Def of (Module.Var.Name.t * t)

let rec to_string node =
  let string_of_def n e  = Module.Var.Name.to_string n ^ ", " ^ to_string e in
  match node with
  | NumLit n -> "NumLit(" ^ string_of_float n ^ ")"
  | StrLit s -> "StrLit(" ^ s ^ ")"
  | SymLit s -> "SymLit(" ^ s ^ ")"
  | Def (n, e) -> "Def(" ^ string_of_def n e ^ ")"
