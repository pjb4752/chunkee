type t =
  | SyntaxError of string
  | ParseError of string
  | ModuleError of string
  | NameError of string
  | TypeError of string

val message: t -> string

val to_string: t -> string
