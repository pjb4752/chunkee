type payload_t = { line_number: int; char_number: int; prefix: string; message: string }

type t = private
  | SyntaxError of payload_t
  | ParseError of payload_t
  | DefinitionError of payload_t
  | NameError of payload_t
  | TypeError of payload_t

val message: t -> string

val to_string: t -> string

val create_syntax_error: Stream_position.t -> string -> t

val create_parse_error: Stream_position.t -> string list -> t

val create_definition_error: Stream_position.t -> string list -> t

val create_name_error: Stream_position.t -> string list -> t

val create_type_error: Stream_position.t -> string list -> t
