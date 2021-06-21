open Printf

type t = {
  source: string;
  line_number: int;
  char_number: int;
}

let inspect { source; line_number; char_number } =
  sprintf "{ source = %s; line_number = %d; char_number = %d }" source line_number char_number
