open Printf

type t = {
  line_number: int;
  char_number: int;
}

let inspect { line_number; char_number } =
  sprintf "{ line_number = %d; char_number = %d }" line_number char_number
