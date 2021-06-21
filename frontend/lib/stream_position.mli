type t = {
  source: string;
  line_number: int;
  char_number: int;
}

val inspect: t -> string
