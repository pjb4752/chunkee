type t =
  | SyntaxError of string
  | ParseError of string
  | NameError of string

val message: t -> string

val to_string: t -> string