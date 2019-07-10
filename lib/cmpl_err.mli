type ert = { line_num: int; char_num: int; message: string }

type t =
  | SyntaxError of ert
  | ParseError of ert
  | DefinitionError of ert
  | NameError of ert
  | TypeError of ert

val message: t -> string

val to_string: t -> string

val parse_errors: Metadata.t -> string list -> t

val parse_error: Metadata.t -> string -> t

val definition_errors: Metadata.t -> string list -> t

val definition_error: Metadata.t -> string -> t

val name_errors: Metadata.t -> string list -> t

val name_error: Metadata.t -> string -> t

val type_errors: Metadata.t -> string list -> t

val type_error: Metadata.t -> string -> t
