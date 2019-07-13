open Printf
open Thwack.Extensions

module Read_list = Thwack.Read_list

exception SyntaxError of string * int * int

module Form = struct
  type t =
    | Number of float * Metadata.t
    | String of string * Metadata.t
    | Symbol of string * Metadata.t
    | Cons of string * Metadata.t
    | List of t list * Metadata.t
    | Vec of t list * Metadata.t

  let rec to_string form =
    let string_of_list l = String.concat " " (List.map to_string l) in
    match form with
    | Number (n, _) -> sprintf "%.2f" n
    | String (s, _) -> sprintf "\"%s\"" s
    | Symbol (s, _) -> sprintf "%s" s
    | Cons (s, _) -> sprintf "%s" s
    | List (l, _) -> sprintf "(%s)" (string_of_list l)
    | Vec (v, _) -> sprintf "[%s]" (string_of_list v)

  let metadata = function
    | Number (_, meta) -> meta
    | String (_, meta) -> meta
    | Symbol (_, meta) -> meta
    | Cons (_, meta) -> meta
    | List (_, meta) -> meta
    | Vec (_, meta) -> meta

  let debug_string form =
    let string_of_list l = String.concat " " (List.map to_string l) in
    match form with
    | Number (n, _) -> sprintf "(number %.2f)" n
    | String (s, _) -> sprintf "(string %s)" s
    | Symbol (s, _) -> sprintf "(symbol %s)" s
    | Cons (s, _) -> sprintf "(cons %s)" s
    | List (l, _) -> sprintf "(list %s)" (string_of_list l)
    | Vec (v, _) -> sprintf "(vec %s)" (string_of_list v)
end

let whitespace = [' '; '\t'; '\n']
let digits = ['0'; '1'; '2'; '3'; '4'; '5'; '6'; '7'; '8'; '9']
let lower_case = ['a'; 'b'; 'c'; 'd'; 'e'; 'f'; 'g'; 'h'; 'i'; 'j'; 'k'; 'l';
  'm'; 'n'; 'o'; 'p'; 'q'; 'r'; 's'; 't'; 'u'; 'v'; 'w'; 'x'; 'y'; 'z']
let upper_case = ['A'; 'B'; 'C'; 'D'; 'E'; 'F'; 'G'; 'H'; 'I'; 'J'; 'K'; 'L';
  'M'; 'N'; 'O'; 'P'; 'Q'; 'R'; 'S'; 'T'; 'U'; 'V'; 'W'; 'X'; 'Y'; 'Z']
let operators = ['+'; '-'; '*'; '/'; '>'; '<'; '=']
let punctuation = ['!'; '?'; '.'; ':']

let symbol_starting_chars = '_' :: (List.append lower_case operators)
let symbol_chars =
  symbol_starting_chars @ upper_case @ digits @ punctuation

let cons_starting_chars = upper_case
let cons_chars = cons_starting_chars @ lower_case @ digits

let is_char_of set chr = List.exists (fun c -> c = chr) set

let is_blank = is_char_of whitespace
let is_digit = is_char_of digits
let is_string_delim = is_char_of ['"']
let is_symbol_starting_char = is_char_of symbol_starting_chars
let is_symbol_char = is_char_of symbol_chars
let is_cons_starting_char = is_char_of cons_starting_chars
let is_cons_char = is_char_of cons_chars
let is_list_open = is_char_of ['(']
let is_vec_open = is_char_of ['[']

let rec remove_blank input =
  Read_list.(
    match input with
    | [] -> []
    | x :: xs when not (is_blank x.value) -> x :: xs
    | _ :: xs -> remove_blank xs
  )

let lex_form input terminal_fn test_fn =
  Read_list.(
    let rec lex_form' input output =
      match input with
      | [] -> terminal_fn input output
      | { value; _ } :: _ when test_fn value -> terminal_fn input output
      | { value; _ } :: xs -> lex_form' xs (String.append_char output value) in
    lex_form' input ""
  )

(* TODO: needs better handling of decimal points, preceding +/- *)
let lex_num line_num char_num input =
  let terminal_fn input output =
    let meta = { Metadata.line_num; char_num } in
    (input, Form.Number (float_of_string output, meta)) in
  let is_not_digit = (fun c -> not (is_digit c)) in
  lex_form input terminal_fn is_not_digit

let lex_symbol line_num char_num input =
  let terminal_fn input output =
    let meta = { Metadata.line_num; char_num } in
    (input, Form.Symbol (output, meta)) in
  let is_not_symbol_char = (fun c -> not (is_symbol_char c)) in
  lex_form input terminal_fn is_not_symbol_char

let lex_cons line_num char_num input =
  let terminal_fn input output =
    let meta = { Metadata.line_num; char_num } in
    (input, Form.Cons (output, meta)) in
  let is_not_cons_char = (fun c -> not (is_cons_char c)) in
  lex_form input terminal_fn is_not_cons_char

let lex_delimited line_num char_num input start delimiter terminal_fn input_fn =
  Read_list.(
    let rec lex_delimited' input output =
      match input with
      | [] ->
          let message = sprintf "unclosed expression found, expecting delimiter '%c'" delimiter in
          raise (SyntaxError (message, line_num, char_num))
      | { value; _ } :: xs when value = delimiter -> (xs, terminal_fn output)
      | _ ->
          let (new_input, new_output) = input_fn input output in
          lex_delimited' new_input new_output in
    lex_delimited' input start
  )

let lex_string line_num char_num input =
  Read_list.(
    let terminal_fn output =
      Form.String (output, { line_num; char_num }) in
    let input_fn input out =
      match input with
      | [] -> assert false
      | { value; _ } :: xs -> (xs, String.append_char out value) in
    match input with
    | [] -> assert false
    | { line_num; char_num; _ } :: xs ->
        lex_delimited line_num char_num xs "" '"' terminal_fn input_fn
  )

let lex_collection lex_fn input terminal_fn final_char =
  Read_list.(
    let input_fn = (fun input output ->
      match input with
      | [] -> assert false
      | { value; _ } :: _ when (is_blank value) ->
          let new_input = remove_blank input in (new_input, output)
      | _ ->
          let (new_input, form) = lex_fn input in
          (new_input, form :: output)) in
    match input with
    | [] -> assert false
    | { line_num; char_num; _ } :: xs ->
        lex_delimited line_num char_num xs [] final_char terminal_fn input_fn
  )

let lex_list lex_fn line_num char_num input =
  let terminal_fn output =
    let meta = { Metadata.line_num; char_num } in
    Form.List (List.rev output, meta) in
  lex_collection lex_fn input terminal_fn ')'

let lex_vec lex_fn line_num char_num input =
  let terminal_fn output =
    let meta = { Metadata.line_num; char_num } in
    Form.Vec (List.rev output, meta) in
  lex_collection lex_fn input terminal_fn ']'

let handle_unexpected_input line_num char_num input =
  let chars = Read_list.(List.map (fun e -> e.value) input) in
  let message = match chars with
  | ')' :: _ -> "found ')' where none was expected"
  | ']' :: _ -> "found ']' where none was expected"
  | _ -> sprintf "unrecognized form '%s'" (String.from_chars chars)
  in
  raise (SyntaxError (message, line_num, char_num))

let rec try_lex input =
  let { Read_list.value; line_num; char_num } = List.hd input in
  if is_digit value then lex_num line_num char_num input
  else if is_string_delim value then lex_string line_num char_num input
  else if is_symbol_starting_char value then lex_symbol line_num char_num input
  else if is_cons_starting_char value then lex_cons line_num char_num input
  else if is_list_open value then lex_list try_lex line_num char_num input
  else if is_vec_open value then lex_vec try_lex line_num char_num input
  else handle_unexpected_input line_num char_num input

let lex_forms input =
  Read_list.(
    let rec lex_list' input forms =
      match input with
      | [] -> List.rev forms
      | { value; _ } :: xs when is_blank value -> lex_list' xs forms
      | _ ->
          let (new_input, new_form) = try_lex input in
          lex_list' new_input (new_form :: forms) in
    lex_list' input []
  )

let lex str =
  try Ok (lex_forms @@ Read_list.from_string str)
  with SyntaxError (message, line_num, char_num) ->
    Error (Cmpl_err.syntax_error line_num char_num message)
