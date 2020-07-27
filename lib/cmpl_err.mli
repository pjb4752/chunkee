type ert = { line_num: int; char_num: int; prefix: string; message: string }

type t =
  | SyntaxError of ert
  | ParseError of ert
  | DefinitionError of ert
  | NameError of ert
  | TypeError of ert

val message: t -> string

val to_string: t -> string

val syntax_error: int -> int -> string -> t

val parse_errors: Metadata.t -> string -> string list -> t

val definition_errors: Metadata.t -> string -> string list -> t

val name_errors: Metadata.t -> string -> string list -> t

val type_errors: Metadata.t -> string -> string list -> t
