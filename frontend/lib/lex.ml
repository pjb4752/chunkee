open Printf
open Common.Extensions

module Read_list = Common.Read_list

exception SyntaxError of int * int * string

module Form = struct
  type t =
    | Number of Metadata.t * string * float
    | String of Metadata.t * string * string
    | Symbol of Metadata.t * string * string
    | List of Metadata.t * string * t list
    | Vector of Metadata.t * string * t list
    | Record of Metadata.t * string * t list
    | Extension of Metadata.t * string * t

  let metadata = function
    | Number (metadata, _, _) -> metadata
    | String (metadata, _, _) -> metadata
    | Symbol (metadata, _, _) -> metadata
    | List (metadata, _, _) -> metadata
    | Vector (metadata, _, _) -> metadata
    | Record (metadata, _, _) -> metadata
    | Extension (metadata, _, _) -> metadata

  let raw_source = function
    | Number (_, raw_source, _) -> raw_source
    | String (_, raw_source, _) -> raw_source
    | Symbol (_, raw_source, _) -> raw_source
    | List (_, raw_source, _) -> raw_source
    | Vector (_, raw_source, _) -> raw_source
    | Record (_, raw_source, _) -> raw_source
    | Extension (_, raw_source, _) -> raw_source

  let rec inspect form =
    let inspect_list l = String.concat " " (List.map inspect l) in
    match form with
    | Number (metadata, raw_source, value) ->
        sprintf "Number(%s, %s, %.5f)" (Metadata.inspect metadata) raw_source value
    | String (metadata, raw_source, value) ->
        sprintf "String(%s, %s, %s)" (Metadata.inspect metadata) raw_source value
    | Symbol (metadata, raw_source, value) ->
        sprintf "Symbol(%s, %s, %s)" (Metadata.inspect metadata) raw_source value
    | List (metadata, raw_source, value) ->
        sprintf "List(%s, %s, %s)" (Metadata.inspect metadata) raw_source (inspect_list value)
    | Vector (metadata, raw_source, value) ->
        sprintf "Vector(%s, %s, %s)" (Metadata.inspect metadata) raw_source (inspect_list value)
    | Record (metadata, raw_source, value) ->
        sprintf "Record(%s, %s, %s)" (Metadata.inspect metadata) raw_source (inspect_list value)
    | Extension (metadata, raw_source, value) ->
        sprintf "Extension(%s, %s, %s)" (Metadata.inspect metadata) raw_source (inspect value)
end

module Result = struct
  type t = (Form.t list, Cmpl_err.t) result

  let inspect result =
    let forms_to_string forms =
      String.concat "; " @@ List.map Form.inspect forms
    in
    Result.inspect result forms_to_string Cmpl_err.to_string
end

type 'a lex_config_t = {
  starting_value: 'a;
  delimiter: char;
  form_builder: ('a -> string -> Form.t);
  input_handler: (Read_list.t -> string -> 'a -> (Read_list.t * string * 'a))
}

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

let remove_blank input =
  let rec remove_blank' input removed_chars =
    match input with
    | [] -> ([], removed_chars)
    | current_char :: remaining_chars when not (is_blank_char Read_list.(current_char.value)) ->
        (current_char :: remaining_chars, removed_chars)
    | removed_char :: remaining_chars ->
        remove_blank' remaining_chars (removed_char.value :: removed_chars)
  in
  remove_blank' input []

let lex_simple_form input_chars form_builder char_predicate =
  let assemble_result input_chars output_string =
    (input_chars, output_string, form_builder output_string)
  in
  let rec lex_simple_form' input_chars output_string =
    match input_chars with
    | [] -> assemble_result input_chars output_string
    | { Read_list.value; _ } :: _ when char_predicate value -> assemble_result input_chars output_string
    | { Read_list.value; _ } :: remaining_chars -> begin
      let output_string = String.append_char output_string value in
      lex_simple_form' remaining_chars output_string
    end
  in
  lex_simple_form' input_chars ""

(* TODO: needs better handling of decimal points, preceding +/- *)
let lex_number line_num char_num input_chars =
  let form_builder output_string =
    let metadata = { Metadata.line_num; char_num } in
    Form.Number (metadata, output_string, float_of_string output_string)
  in
  let is_not_digit = (fun c -> not (is_digit_char c)) in
  lex_simple_form input_chars form_builder is_not_digit

let lex_symbol line_num char_num input_chars =
  let form_builder output_string =
    let metadata = { Metadata.line_num; char_num } in
    Form.Symbol (metadata, output_string, output_string)
  in
  let is_not_symbol_char = (fun c -> not (is_symbol_char c)) in
  lex_simple_form input_chars form_builder is_not_symbol_char

let lex_delimited line_num char_num input_chars raw_source config =
  let rec lex_delimited' input_chars raw_source output =
    match input_chars with
    | [] ->
        let message = sprintf "unclosed expression found, expecting delimiter '%c'" config.delimiter in
        raise (SyntaxError (line_num, char_num, message))
    | { Read_list.value; _ } :: remaining_chars when value = config.delimiter ->
        let raw_source = String.append_char raw_source value in
        (remaining_chars, raw_source, config.form_builder output raw_source)
    | _ -> begin
      let (input_chars, raw_source, output) = config.input_handler input_chars raw_source output in
      lex_delimited' input_chars raw_source output
    end
  in
  lex_delimited' input_chars raw_source config.starting_value

let lex_string line_num char_num input_chars =
  let form_builder output_string raw_source =
    Form.String ({ line_num; char_num }, raw_source, output_string)
  in
  let input_handler input_chars raw_source output_string =
    match input_chars with
    | [] -> assert false
    | { Read_list.value; _ } :: remaining_chars -> begin
        let raw_source = String.append_char raw_source value in
        let output_string = String.append_char output_string value in
        (remaining_chars, raw_source, output_string)
    end
  in
  match input_chars with
  | [] -> assert false
  | { Read_list.value; line_num; char_num; } :: remaining_chars -> begin
    let lexer_config = { starting_value = ""; delimiter = '"'; form_builder; input_handler } in
    let raw_source = String.append_char "" value in
    lex_delimited line_num char_num remaining_chars raw_source lexer_config
  end

let lex_collection recursively_lex input_chars form_builder final_char =
  let input_handler input_chars raw_source output_forms =
    match input_chars with
    | [] -> assert false
    | { Read_list.value; _ } :: _ when (is_blank_char value) -> begin
        let (input_chars, removed_chars) = remove_blank input_chars in
        let raw_source = List.fold_right (fun removed_char raw_source ->
          String.append_char raw_source removed_char
        ) removed_chars raw_source in
        (input_chars, raw_source, output_forms)
    end
    | _ -> begin
      let (input_chars, sub_raw_source, form) = recursively_lex input_chars in
      (input_chars, String.concat "" [raw_source; sub_raw_source], form :: output_forms)
    end
  in
  match input_chars with
  | [] -> assert false
  | { Read_list.value; line_num; char_num; } :: remaining_chars -> begin
    let lexer_config = { starting_value = []; delimiter = final_char; form_builder; input_handler } in
    let raw_source = String.append_char "" value in
    lex_delimited line_num char_num remaining_chars raw_source lexer_config
  end

(* TODO capture original raw text as part of collection Form*)
let lex_list recursively_lex line_num char_num input_chars =
  let form_builder output_forms raw_source =
    let metadata = { Metadata.line_num; char_num } in
    Form.List (metadata, raw_source, List.rev output_forms)
  in
  lex_collection recursively_lex input_chars form_builder ')'

let lex_vector recursively_lex line_num char_num input_chars =
  let form_builder output_forms raw_source =
    let metadata = { Metadata.line_num; char_num } in
    Form.Vector (metadata, raw_source, List.rev output_forms)
  in
  lex_collection recursively_lex input_chars form_builder ']'

let lex_record recursively_lex line_num char_num input_chars =
  let form_builder output_forms raw_source =
    let metadata = { Metadata.line_num; char_num } in
    Form.Record (metadata, raw_source, List.rev output_forms)
  in
  lex_collection recursively_lex input_chars form_builder '}'

let lex_extension recursively_lex line_num char_num input_chars =
  match input_chars with
  | [] -> begin
    let message = sprintf "unexpected end of input after extension char ^" in
    raise (SyntaxError (line_num, char_num, message))
  end
  | _ :: raw_extension -> begin
    let (input_chars, raw_source, extension_form) = recursively_lex raw_extension in
    let metadata = { Metadata.line_num; char_num } in
    let raw_source = "^" ^ raw_source in
    (input_chars, raw_source, Form.Extension (metadata, raw_source, extension_form))
  end

let handle_unexpected_input line_num char_num input =
  let chars = Read_list.(List.map (fun e -> e.value) input) in
  let message = match chars with
  | ')' :: _ -> "found ')' where none was expected"
  | ']' :: _ -> "found ']' where none was expected"
  | _ -> sprintf "unrecognized form '%s'" (String.from_chars chars)
  in
  raise (SyntaxError (line_num, char_num, message))

let rec try_lex input_chars =
  let recursively_lex = try_lex in
  let { Read_list.value; line_num; char_num } = List.hd input_chars in
  if is_digit_char value then
    lex_number line_num char_num input_chars
  else if is_string_delimiter_char value then
    lex_string line_num char_num input_chars
  else if is_symbol_starting_char value then
    lex_symbol line_num char_num input_chars
  else if is_list_opening_char value then
    lex_list recursively_lex line_num char_num input_chars
  else if is_vector_opening_char value then
    lex_vector recursively_lex line_num char_num input_chars
  else if is_record_opening_char value then
    lex_record recursively_lex line_num char_num input_chars
  else if is_extension_starting_char value then
    lex_extension recursively_lex line_num char_num input_chars
  else
    handle_unexpected_input line_num char_num input_chars

let lex_forms input_chars =
  let rec lex_forms' input_chars forms =
    match input_chars with
    | [] -> List.rev forms
    | { Read_list.value; _ } :: remaining_chars when is_blank_char value -> lex_forms' remaining_chars forms
    | _ -> begin
      let (new_input, _, new_form) = try_lex input_chars in
      lex_forms' new_input (new_form :: forms)
    end
  in
  lex_forms' input_chars []

let lex str =
  try Ok (lex_forms @@ Read_list.from_string str)
  with SyntaxError (line_num, char_num, message) ->
    Error (Cmpl_err.syntax_error line_num char_num message)
