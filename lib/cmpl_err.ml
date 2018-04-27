type t =
  | SyntaxError of string
  | ParseError of string
  | NameError of string
  | TypeError of string

let message = function
  | SyntaxError m -> m
  | ParseError m -> m
  | NameError m -> m
  | TypeError m -> m

let to_string = function
  | SyntaxError m -> "SyntaxError: " ^ m
  | ParseError m -> "ParseError: " ^ m
  | NameError m -> "NameError: " ^ m
  | TypeError m -> "TypeError: " ^ m
