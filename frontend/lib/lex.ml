open Printf
open Common.Extensions

module Read_list = Common.Read_list

exception SyntaxError of int * int * string

module Form = struct
  type t =
    | Number of Metadata.t * float
    | String of Metadata.t * string
    | Symbol of Metadata.t * string
    | List of Metadata.t * t list
    | Vector of Metadata.t * t list
    | Record of Metadata.t * t list
    | Extension of Metadata.t * t

  let metadata = function
    | Number (metadata, _) -> metadata
    | String (metadata, _) -> metadata
    | Symbol (metadata, _) -> metadata
    | List (metadata, _) -> metadata
    | Vector (metadata, _) -> metadata
    | Record (metadata, _) -> metadata
    | Extension (metadata, _) -> metadata

  let rec to_string form =
    let string_of_list l = String.concat " " (List.map to_string l) in
    match form with
    | Number (_, value) -> sprintf "%.2f" value
    | String (_, value) -> sprintf "\"%s\"" value
    | Symbol (_, value) -> sprintf "%s" value
    | List (_, value) -> sprintf "(%s)" (string_of_list value)
    | Vector (_, value) -> sprintf "[%s]" (string_of_list value)
    | Record (_, value) -> sprintf "{%s}" (string_of_list value)
    | Extension (_, value) -> sprintf "^%s" (to_string value)

  let rec inspect form =
    let inspect_list l = String.concat " " (List.map inspect l) in
    match form with
    | Number (metadata, value) -> sprintf "Number(%s, %.2f)" (Metadata.inspect metadata) value
    | String (metadata, value) -> sprintf "String(%s, %s)" (Metadata.inspect metadata) value
    | Symbol (metadata, value) -> sprintf "Symbol(%s, %s)" (Metadata.inspect metadata) value
    | List (metadata, value) -> sprintf "List(%s, %s)" (Metadata.inspect metadata) (inspect_list value)
    | Vector (metadata, value) -> sprintf "Vector(%s, %s)" (Metadata.inspect metadata) (inspect_list value)
    | Record (metadata, value) -> sprintf "Record(%s, %s)" (Metadata.inspect metadata) (inspect_list value)
    | Extension (metadata, value) -> sprintf "Extension(%s, %s)" (Metadata.inspect metadata) (inspect value)
end

type t = (Form.t list, Cmpl_err.t) result

let whitespace = [' '; '\t'; '\n']
let digits = ['0'; '1'; '2'; '3'; '4'; '5'; '6'; '7'; '8'; '9']
let lower_case = ['a'; 'b'; 'c'; 'd'; 'e'; 'f'; 'g'; 'h'; 'i'; 'j'; 'k'; 'l';
  'm'; 'n'; 'o'; 'p'; 'q'; 'r'; 's'; 't'; 'u'; 'v'; 'w'; 'x'; 'y'; 'z']
let operators = ['+'; '-'; '*'; '/'; '>'; '<'; '=']
let punctuation = ['!'; '?'; '.'; ':']

let symbol_starting_chars = '_' :: (List.append lower_case operators)
let symbol_chars = symbol_starting_chars @ digits @ punctuation

let is_char_of set chr = List.exists (fun c -> c = chr) set

let is_blank_char = is_char_of whitespace
let is_digit_char = is_char_of digits
let is_string_delimiter_char = is_char_of ['"']
let is_symbol_starting_char = is_char_of symbol_starting_chars
let is_symbol_char = is_char_of symbol_chars
let is_list_opening_char = is_char_of ['(']
let is_vector_opening_char = is_char_of ['[']
let is_record_opening_char = is_char_of ['{']
let is_extension_starting_char = is_char_of ['^']

let rec remove_blank input =
  match input with
  | [] -> []
  | x :: xs when not (is_blank_char Read_list.(x.value)) -> x :: xs
  | _ :: xs -> remove_blank xs

let lex_form input assemble_form char_matches =
  let rec lex_form' input output =
    match input with
    | [] -> assemble_form input output
    | { Read_list.value; _ } :: _ when char_matches value -> assemble_form input output
    | { Read_list.value; _ } :: xs -> lex_form' xs (String.append_char output value) in
  lex_form' input ""

(* TODO: needs better handling of decimal points, preceding +/- *)
let lex_number line_num char_num input =
  let assemble_form input output =
    let metadata = { Metadata.line_num; char_num } in
    (input, Form.Number (metadata, float_of_string output)) in
  let is_not_digit = (fun c -> not (is_digit_char c)) in
  lex_form input assemble_form is_not_digit

let lex_symbol line_num char_num input =
  let assemble_form input output =
    let metadata = { Metadata.line_num; char_num } in
    (input, Form.Symbol (metadata, output)) in
  let is_not_symbol_char = (fun c -> not (is_symbol_char c)) in
  lex_form input assemble_form is_not_symbol_char

let lex_delimited line_num char_num input start delimiter assemble_form handle_input_char =
  Read_list.(
    let rec lex_delimited' input output =
      match input with
      | [] ->
          let message = sprintf "unclosed expression found, expecting delimiter '%c'" delimiter in
          raise (SyntaxError (line_num, char_num, message))
      | { value; _ } :: xs when value = delimiter -> (xs, assemble_form output)
      | _ ->
          let (new_input, new_output) = handle_input_char input output in
          lex_delimited' new_input new_output in
    lex_delimited' input start
  )

let lex_string line_num char_num input =
  Read_list.(
    let assemble_form output =
      Form.String ({ line_num; char_num }, output) in
    let handle_input_char input out =
      match input with
      | [] -> assert false
      | { value; _ } :: xs -> (xs, String.append_char out value) in
    match input with
    | [] -> assert false
    | { line_num; char_num; _ } :: xs ->
        lex_delimited line_num char_num xs "" '"' assemble_form handle_input_char
  )

let lex_collection lex_fn input assemble_form final_char =
  Read_list.(
    let handle_input_char = (fun input output ->
      match input with
      | [] -> assert false
      | { value; _ } :: _ when (is_blank_char value) ->
          let new_input = remove_blank input in (new_input, output)
      | _ ->
          let (new_input, form) = lex_fn input in
          (new_input, form :: output)) in
    match input with
    | [] -> assert false
    | { line_num; char_num; _ } :: xs ->
        lex_delimited line_num char_num xs [] final_char assemble_form handle_input_char
  )

let lex_list lex_fn line_num char_num input =
  let assemble_form output =
    let metadata = { Metadata.line_num; char_num } in
    Form.List (metadata, List.rev output) in
  lex_collection lex_fn input assemble_form ')'

let lex_vector lex_fn line_num char_num input =
  let assemble_form output =
    let metadata = { Metadata.line_num; char_num } in
    Form.Vector (metadata, List.rev output) in
  lex_collection lex_fn input assemble_form ']'

let lex_record lex_fn line_num char_num input =
  let assemble_form output =
    let metadata = { Metadata.line_num; char_num } in
    Form.Record (metadata, List.rev output) in
  lex_collection lex_fn input assemble_form '}'

let lex_extension lex_fn line_num char_num input =
  match input with
  | [] -> begin
    let message = sprintf "unexpected end of input after extension char ^" in
    raise (SyntaxError (line_num, char_num, message))
  end
  | _ :: raw_extension -> begin
    let (new_input, extension_form) = lex_fn raw_extension in
    let metadata = { Metadata.line_num; char_num } in
    (new_input, Form.Extension (metadata, extension_form))
  end

let handle_unexpected_input line_num char_num input =
  let chars = Read_list.(List.map (fun e -> e.value) input) in
  let message = match chars with
  | ')' :: _ -> "found ')' where none was expected"
  | ']' :: _ -> "found ']' where none was expected"
  | _ -> sprintf "unrecognized form '%s'" (String.from_chars chars)
  in
  raise (SyntaxError (line_num, char_num, message))

let rec try_lex input =
  let { Read_list.value; line_num; char_num } = List.hd input in
  if is_digit_char value then lex_number line_num char_num input
  else if is_string_delimiter_char value then lex_string line_num char_num input
  else if is_symbol_starting_char value then lex_symbol line_num char_num input
  else if is_list_opening_char value then lex_list try_lex line_num char_num input
  else if is_vector_opening_char value then lex_vector try_lex line_num char_num input
  else if is_record_opening_char value then lex_record try_lex line_num char_num input
  else if is_extension_starting_char value then lex_extension try_lex line_num char_num input
  else handle_unexpected_input line_num char_num input

let lex_forms input =
  Read_list.(
    let rec lex_list' input forms =
      match input with
      | [] -> List.rev forms
      | { value; _ } :: xs when is_blank_char value -> lex_list' xs forms
      | _ ->
          let (new_input, new_form) = try_lex input in
          lex_list' new_input (new_form :: forms) in
    lex_list' input []
  )

let lex str =
  try Ok (lex_forms @@ Read_list.from_string str)
  with SyntaxError (line_num, char_num, message) ->
    Error (Cmpl_err.syntax_error line_num char_num message)

let inspect result =
  let forms_to_string forms =
    String.concat "; " @@ List.map Form.inspect forms in
  Result.inspect result forms_to_string Cmpl_err.to_string
