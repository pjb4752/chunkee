type form =
  | Number of float
  | String of string
  | Symbol of string
  | List of form list

let string_of_list l =
  List.fold_left (fun a b -> a ^ b) "" l

let rec to_string = function
  | Number n -> "Number(" ^ string_of_float n ^ ")"
  | String s -> "String(" ^ s ^ ")"
  | Symbol s -> "Symbol(" ^ s ^ ")"
  | List l -> "List(" ^ string_of_list (List.map to_string l) ^ ")"
