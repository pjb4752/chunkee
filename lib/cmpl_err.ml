type ert = { line_num: int; char_num: int; message: string }

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

let message_to_string prefix { line_num; char_num; message } =
  Printf.sprintf
    "%s:\n%s\n\tError found at %d:%d" prefix message line_num char_num

let to_string = function
  | SyntaxError payload -> message_to_string "Syntax Error" payload
  | ParseError payload -> message_to_string "Parse Error" payload
  | DefinitionError payload -> message_to_string "Definition Error" payload
  | NameError payload -> message_to_string "Name Error" payload
  | TypeError payload -> message_to_string "Type Error" payload

let parse_errors { Metadata.line_num; char_num } messages =
  let message = String.concat "\n" messages in
  ParseError { line_num; char_num; message }

let parse_error metadata message =
  parse_errors metadata [message]

let definition_errors { Metadata.line_num; char_num } messages =
  let message = String.concat "\n" messages in
  DefinitionError { line_num; char_num; message }

let definition_error metadata message =
  definition_errors metadata [message]

let name_errors { Metadata.line_num; char_num } messages =
  let message = String.concat "\n" messages in
  NameError { line_num; char_num; message }

let name_error metadata message =
  name_errors metadata [message]

let type_errors { Metadata.line_num; char_num } messages =
  let message = String.concat "\n" messages in
  TypeError { line_num; char_num; message }

let type_error metadata message =
  type_errors metadata [message]
