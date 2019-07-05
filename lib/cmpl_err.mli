type ert = { line_num: int; char_num: int; message: string }

type t =
  | SyntaxError of ert
  | ParseError of ert
  | DefinitionError of ert
  | ModuleError of string
  | NameError of string
  | TypeError of string

val message: t -> string

val to_string: t -> string
