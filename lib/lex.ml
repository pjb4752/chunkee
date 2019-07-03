open Printf
open Thwack.Extensions

exception SyntaxError of string

module Form = struct
  type t =
    | Number of float
    | String of string
    | Symbol of string
    | Cons of string
    | List of t list
    | Vec of t list

  let rec to_string form =
    let string_of_list l = String.concat " " (List.map to_string l) in
    match form with
    | Number n -> sprintf "(number %.2f)" n
    | String s -> sprintf "(string %s)" s
    | Symbol s -> sprintf "(symbol %s)" s
    | Cons s -> sprintf "(cons %s)" s
    | List l -> sprintf "(list %s)" (string_of_list l)
    | Vec v -> sprintf "(vec %s)" (string_of_list v)
end

let whitespace = [' '; '\t'; '\n']
let digits = ['0'; '1'; '2'; '3'; '4'; '5'; '6'; '7'; '8'; '9']
let lower_case = ['a'; 'b'; 'c'; 'd'; 'e'; 'f'; 'g'; 'h'; 'i'; 'j'; 'k'; 'l';
  'm'; 'n'; 'o'; 'p'; 'q'; 'r'; 's'; 't'; 'u'; 'v'; 'w'; 'x'; 'y'; 'z']
let upper_case = ['A'; 'B'; 'C'; 'D'; 'E'; 'F'; 'G'; 'H'; 'I'; 'J'; 'K'; 'L';
  'M'; 'N'; 'O'; 'P'; 'Q'; 'R'; 'S'; 'T'; 'U'; 'V'; 'W'; 'X'; 'Y'; 'Z']
let operators = ['+'; '-'; '*'; '/'; '>'; '<'; '=']

let symbol_starting_chars = '_' :: (List.append lower_case operators)
let symbol_chars =
  symbol_starting_chars @ upper_case @ digits @ ['!'; '?'; '.'; ':']

let cons_starting_chars = upper_case
let cons_chars =
  cons_starting_chars @ lower_case @ digits

let is_char_of set ch = List.exists (fun c -> c = ch) set

let is_blank = is_char_of whitespace
let is_digit = is_char_of digits
let is_string_delim = is_char_of ['"']
let is_symbol_starting_char = is_char_of symbol_starting_chars
let is_symbol_char = is_char_of symbol_chars
let is_cons_starting_char = is_char_of cons_starting_chars
let is_cons_char = is_char_of cons_chars
let is_list_open = is_char_of ['(']
let is_vec_open = is_char_of ['[']

let string_to_number s = Form.Number (float_of_string s)

let rec remove_blank = function
  | [] -> []
  | x :: xs when not (is_blank x) -> x :: xs
  | _ :: xs -> remove_blank xs

let lex_form input terminal_fn test_fn =
  let rec lex_form' input output =
    match input with
    | [] -> terminal_fn input output
    | x :: _ when test_fn x -> terminal_fn input output
    | x :: xs -> lex_form' xs (String.append_char output x) in
  lex_form' input ""

(* TODO: needs better handling of decimal points, preceding +/- *)
let lex_num input =
  let terminal_fn i o = (i, string_to_number o)
  and is_not_digit = (fun c -> not (is_digit c)) in
  lex_form input terminal_fn is_not_digit

let lex_symbol input =
  let terminal_fn i o = (i, Form.Symbol o)
  and is_not_symbol_char = (fun c -> not (is_symbol_char c)) in
  lex_form input terminal_fn is_not_symbol_char

let lex_cons input =
  let terminal_fn i o = (i, Form.Cons o)
  and is_not_cons_char = (fun c -> not (is_cons_char c)) in
  lex_form input terminal_fn is_not_cons_char

let lex_delimited input start delimiter terminal_fn input_fn =
  let rec lex_delimited' input output =
    match input with
    | [] -> raise (SyntaxError (Printf.sprintf "expecting '%c', none found" delimiter))
    | x :: xs when x = delimiter -> (xs, terminal_fn output)
    | _ :: _ ->
        let (new_input, new_output) = input_fn input output in
        lex_delimited' new_input new_output in
  lex_delimited' input start

let lex_string input =
  let terminal_fn = (fun out -> Form.String out)
  and input_fn i out =
    match i with
    | [] -> assert false
    | x :: xs -> (xs, String.append_char out x) in
  lex_delimited (List.tl input) "" '"' terminal_fn input_fn

let lex_collection lex_fn input terminal_fn terminal_char =
  let input_fn = (fun i out ->
    match i with
    | [] -> assert false
    | x :: _ when (is_blank x) -> let new_i = remove_blank i in (new_i, out)
    | _ :: _ -> let (new_i, f) = lex_fn i in (new_i, f :: out)) in
  lex_delimited (List.tl input) [] terminal_char terminal_fn input_fn

let lex_list lex_fn input =
  let terminal_fn = (fun out -> Form.List (List.rev out)) in
  lex_collection lex_fn input terminal_fn ')'

let lex_vec lex_fn input =
  let terminal_fn = (fun out -> Form.Vec (List.rev out)) in
  lex_collection lex_fn input terminal_fn ']'

let rec try_lex input =
  let c = List.hd input in
  if is_digit c then lex_num input
  else if is_string_delim c then lex_string input
  else if is_symbol_starting_char c then lex_symbol input
  else if is_cons_starting_char c then lex_cons input
  else if is_list_open c then lex_list try_lex input
  else if is_vec_open c then lex_vec try_lex input
  else let bad_input = String.from_chars input in
    raise (SyntaxError (sprintf "unrecognized form '%s'" bad_input))

let lex_forms input =
  let rec lex_list' input forms =
    match input with
    | [] -> List.rev forms
    | x :: xs when is_blank x -> lex_list' xs forms
    | _ ->
        let (new_input, new_form) = try_lex input in
        lex_list' new_input (new_form :: forms) in
  lex_list' input []

let lex_exn s = lex_forms @@ String.to_chars s

let lex s =
  try Ok (lex_exn s)
  with SyntaxError e -> Error (Cmpl_err.SyntaxError e)
