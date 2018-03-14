type form =
  | Number of float
  | String of string
  | Symbol of string

val to_string: form -> string
