open Printf
open Common.Extensions

module Read_list = Common.Read_list

exception SyntaxError of int * int * string

module Form = struct
  type t = {
    metadata: Metadata.t;
    lexed: u
  }
  and u =
    | Number of float
    | String of string
    | Symbol of string
    | List of (t list)
    | Vector of (t list)
    | Extension of t

  let metadata form = form.metadata

  let source form = form.metadata.source

  let rec inspect { metadata; lexed } =
    let metadata = Metadata.inspect metadata in
    let inspect_list l = String.concat " " (List.map inspect l) in
    match lexed with
    | Number value -> sprintf "Number(%s, %.5f)" metadata value
    | String value -> sprintf "String(%s, %s)" metadata value
    | Symbol value -> sprintf "Symbol(%s, %s)" metadata value
    | List value -> sprintf "List(%s, %s)" metadata (inspect_list value)
    | Vector value -> sprintf "Vector(%s, %s)" metadata (inspect_list value)
    | Extension value -> sprintf "Extension(%s, %s)" metadata (inspect value)
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
let upper_case = ['A'; 'B'; 'C'; 'D'; 'E'; 'F'; 'G'; 'H'; 'I'; 'J'; 'K'; 'L';
  'M'; 'N'; 'O'; 'P'; 'Q'; 'R'; 'S'; 'T'; 'U'; 'V'; 'W'; 'X'; 'Y'; 'Z']
let operators = ['+'; '-'; '*'; '/'; '>'; '<'; '=']
let punctuation = ['!'; '?'; '.'; ':']

let symbol_starting_chars = '_' :: (List.append (List.append lower_case operators) upper_case)
let symbol_chars = symbol_starting_chars @ digits @ punctuation

let is_char_of set chr = List.exists (fun c -> c = chr) set

let is_blank_char = is_char_of whitespace
let is_digit_char = is_char_of digits
let is_string_delimiter_char = is_char_of ['"']
let is_symbol_starting_char = is_char_of symbol_starting_chars
let is_symbol_char = is_char_of symbol_chars
let is_list_opening_char = is_char_of ['(']
let is_vector_opening_char = is_char_of ['[']
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
    let metadata = { Metadata.line_num; char_num; source = output_string } in
    let lexed = Form.Number (float_of_string output_string) in
    { Form.metadata; lexed }
  in
  let is_not_digit = (fun c -> not (is_digit_char c)) in
  lex_simple_form input_chars form_builder is_not_digit

let lex_symbol line_num char_num input_chars =
  let form_builder output_string =
    let metadata = { Metadata.line_num; char_num; source = output_string } in
    let lexed = Form.Symbol output_string in
    { Form.metadata; lexed }
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
    let metadata = { Metadata.line_num; char_num; source = raw_source } in
    let lexed = Form.String output_string in
    { Form.metadata; lexed }
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
    let metadata = { Metadata.line_num; char_num; source = raw_source } in
    let lexed = Form.List (List.rev output_forms) in
    { Form.metadata; lexed }
  in
  lex_collection recursively_lex input_chars form_builder ')'

let lex_vector recursively_lex line_num char_num input_chars =
  let form_builder output_forms raw_source =
    let metadata = { Metadata.line_num; char_num; source = raw_source } in
    let lexed = Form.Vector (List.rev output_forms) in
    { Form.metadata; lexed }
  in
  lex_collection recursively_lex input_chars form_builder ']'

let lex_extension recursively_lex line_num char_num input_chars =
  match input_chars with
  | [] -> begin
    let message = sprintf "unexpected end of input after extension char ^" in
    raise (SyntaxError (line_num, char_num, message))
  end
  | _ :: raw_extension -> begin
    let (input_chars, raw_source, extension_form) = recursively_lex raw_extension in
    let raw_source = "^" ^ raw_source in
    let metadata = { Metadata.line_num; char_num; source = raw_source } in
    let lexed = Form.Extension extension_form in
    (input_chars, raw_source, { Form.metadata; lexed })
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
