type ert = { line_num: int; char_num: int; message: string }

type t =
  | SyntaxError of ert
  | ParseError of ert
  | ModuleError of string
  | NameError of string
  | TypeError of string

let message = function
  | SyntaxError { message } -> message
  | ParseError { message } -> message
  | ModuleError message -> message
  | NameError message -> message
  | TypeError message -> message

let message_to_string prefix { line_num; char_num; message } =
  Printf.sprintf
    "%s:\n%s\n\tError found at %d:%d" prefix message line_num char_num

let to_string = function
  | SyntaxError payload -> message_to_string "Syntax Error" payload
  | ParseError payload -> message_to_string "Parse Error" payload
  | ModuleError payload -> "ModuleError: " ^ payload
  | NameError payload -> "NameError: " ^ payload
  | TypeError payload -> "TypeError: " ^ payload
