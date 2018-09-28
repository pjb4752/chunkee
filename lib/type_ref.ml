open Printf

type t =
  | StrType of string
  | FnType of t list

let from_string s = StrType s

let from_list l = FnType l

let rec to_string t =
  let string_of_fn l =
    let l = List.map to_string l in
    sprintf "[%s]" (String.concat " " l) in
  match t with
  | StrType s -> s
  | FnType l -> string_of_fn l
