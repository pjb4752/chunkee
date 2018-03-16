type t =
  | Number of float
  | String of string
  | Symbol of string
  | List of t list

let rec to_string form =
  let string_of_list l = String.concat ", " (List.map to_string l) in
  match form with
  | Number n -> "Number(" ^ string_of_float n ^ ")"
  | String s -> "String(" ^ s ^ ")"
  | Symbol s -> "Symbol(" ^ s ^ ")"
  | List l -> "List(" ^ string_of_list l ^ ")"
