type form =
  | Number of float
  | String of string
  | Symbol of string

let to_string = function
  | Number n -> "Number(" ^ string_of_float n ^ ")"
  | String s -> "String(" ^ s ^ ")"
  | Symbol s -> "Symbol(" ^ s ^ ")"
