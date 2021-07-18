type payload_t = { line_number: int; char_number: int; prefix: string; message: string }

type t =
  | SyntaxError of payload_t
  | ParseError of payload_t
  | DefinitionError of payload_t
  | NameError of payload_t
  | TypeError of payload_t

val message: t -> string

val to_string: t -> string

val syntax_error: int -> int -> string -> t

val parse_errors: Stream_position.t -> string list -> t

val definition_errors: Stream_position.t -> string list -> t

val name_errors: Stream_position.t -> string list -> t

val type_errors: Stream_position.t -> string list -> t
