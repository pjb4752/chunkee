open Printf

type t =
  | Number of float
  | String of string
  | Symbol of string
  | List of t list

let rec to_string form =
  let string_of_list l = String.concat " " (List.map to_string l) in
  match form with
  | Number n -> sprintf "(number %.2f)" n
  | String s -> sprintf "(string %s)" s
  | Symbol s -> sprintf "(symbol %s)" s
  | List l -> sprintf "(list %s)" (string_of_list l)
