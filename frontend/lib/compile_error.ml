open Printf

type payload_t = { line_number: int; char_number: int; prefix: string; message: string }

type t =
  | SyntaxError of payload_t
  | ParseError of payload_t
  | DefinitionError of payload_t
  | NameError of payload_t
  | TypeError of payload_t

let message = function
  | SyntaxError { message; _ } -> message
  | ParseError { message; _ } -> message
  | DefinitionError { message; _ } -> message
  | NameError { message; _ } -> message
  | TypeError { message; _ } -> message

let message_to_string name { prefix; message; _ } =
  sprintf "%s: %s\n\t%s" name prefix message

let to_string = function
  | SyntaxError payload -> message_to_string "Syntax Error" payload
  | ParseError payload  -> message_to_string "Parse Error" payload
  | DefinitionError payload -> message_to_string "Definition Error" payload
  | NameError payload -> message_to_string "Name Error" payload
  | TypeError payload -> message_to_string "Type Error" payload

let build_message_parts messages line_number char_number =
  (String.concat "" messages, sprintf "in expression at %d:%d" line_number char_number)

let create_syntax_error { Stream_position.line_number; char_number; _ } message =
  let prefix = sprintf "in expression at %d:%d" line_number char_number in
  SyntaxError { line_number; char_number; prefix; message }

let create_parse_error { Stream_position.line_number; char_number; _ } message_fragments =
  let (message, prefix) = build_message_parts message_fragments line_number char_number in
  ParseError { line_number; char_number; prefix; message }

let create_definition_error { Stream_position.line_number; char_number; _ } message_fragments =
  let (message, prefix) = build_message_parts message_fragments line_number char_number in
  DefinitionError { line_number; char_number; prefix; message }

let create_name_error { Stream_position.line_number; char_number; _ } message_fragments =
  let (message, prefix) = build_message_parts message_fragments line_number char_number in
  NameError { line_number; char_number; prefix; message }

let create_type_error { Stream_position.line_number; char_number; _ } message_fragments =
  let (message, prefix) = build_message_parts message_fragments line_number char_number in
  TypeError { line_number; char_number; prefix; message }
