open Printf

type ert = { line_num: int; char_num: int; prefix: string; message: string }

type t =
  | SyntaxError of ert
  | ParseError of ert
  | DefinitionError of ert
  | NameError of ert
  | TypeError of ert

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

let syntax_error line_num char_num message =
  let prefix = sprintf "in expression at %d:%d" line_num char_num in
  SyntaxError { line_num; char_num; prefix; message }

let parse_errors { Metadata.line_num; char_num; _ } prefix messages =
  let message = String.concat "" messages in
  ParseError { line_num; char_num; prefix; message }

let definition_errors { Metadata.line_num; char_num; _ } prefix messages =
  let message = String.concat "" messages in
  DefinitionError { line_num; char_num; prefix; message }

let name_errors { Metadata.line_num; char_num; _ } prefix messages =
  let message = String.concat "" messages in
  NameError { line_num; char_num; prefix; message }

let type_errors { Metadata.line_num; char_num; _ } prefix messages =
  let message = String.concat "" messages in
  TypeError { line_num; char_num; prefix; message }
