open Printf

type t = {
  position: Stream_position.t;
  value: u
} and u =
  | Number of float
  | String of string
  | Symbol of string
  | List of t list
  | Vector of t list


let create_number position value =
  { position; value = Number value }

let create_string position value =
  { position; value = String value }

let create_symbol position value =
  { position; value = Symbol value }

let create_list position value =
  { position; value = List value }

let inspect form =
  let rec inspect' { position; value; _ } =
    let position = Stream_position.inspect position in
    let value = match value with
    | Number n -> sprintf "Source_form.Number(%f)" n
    | String s -> sprintf "Source_form.String(\"%s\")" s
    | Symbol s -> sprintf "Source_form.Symbol(%s)" s
    | List l -> sprintf "Source_form.List(%s)" (String.concat " " (List.map inspect' l))
    | Vector v -> sprintf "Source_form.Vector(%s)" (String.concat " " (List.map inspect' v))
    in sprintf "{ position = %s; value = %s}" position value
  in
  inspect' form
