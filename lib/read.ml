exception SyntaxError of string

let whitespace = [' '; '\t'; '\n']
let digits = ['0'; '1'; '2'; '3'; '4'; '5'; '6'; '7'; '8'; '9']
let lower_case = ['a'; 'b'; 'c'; 'd'; 'e'; 'f'; 'g'; 'h'; 'i'; 'j'; 'k'; 'l';
  'm'; 'n'; 'o'; 'p'; 'q'; 'r'; 's'; 't'; 'u'; 'v'; 'w'; 'x'; 'y'; 'z']
let upper_case = ['A'; 'B'; 'C'; 'D'; 'E'; 'F'; 'G'; 'H'; 'I'; 'J'; 'K'; 'L';
  'M'; 'N'; 'O'; 'P'; 'Q'; 'R'; 'S'; 'T'; 'U'; 'V'; 'W'; 'X'; 'Y'; 'Z']
let operators = ['+'; '-'; '*'; '/'; '>'; '<'; '=']
let alpha = '_' :: (List.append lower_case upper_case)

let symbol_starting_chars = List.append alpha operators
let symbol_chars =
  List.append (List.append symbol_starting_chars digits) ['!'; '?'; '.']

let append_char s c = s ^ String.make 1 c

let explode s =
  let rec exp i l =
    if i < 0 then l else exp (i - 1) (s.[i] :: l) in
  exp (String.length s - 1) []

let implode l = List.fold_left append_char "" l

let is_char_of set ch = List.exists (fun c -> c = ch) set

let is_blank = is_char_of whitespace
let is_digit = is_char_of digits
let is_string_delim = is_char_of ['"']
let is_symbol_starting_char = is_char_of symbol_starting_chars
let is_symbol_char = is_char_of symbol_chars

let string_to_number s = Form.Number (float_of_string s)

let read_form input terminal_fn test_fn =
  let rec read_form' input output =
    match input with
    | [] -> terminal_fn input output
    | x :: xs when test_fn x -> terminal_fn input output
    | x :: xs -> read_form' xs (append_char output x) in
  read_form' input ""

(* TODO: needs better handling of decimal points, preceding +/- *)
let read_num input =
  let terminal_fn i o = (i, string_to_number o) in
    let is_not_digit = (fun c -> not (is_digit c)) in
      read_form input terminal_fn is_not_digit

let read_string input =
  let rec read_string' input output =
    match input with
    | [] -> raise (SyntaxError "expecting '\"', none found")
    | x :: xs when (is_string_delim x) -> (xs, Form.String output)
    | x :: xs -> read_string' xs (append_char output x) in
  read_string' (List.tl input) ""

let read_symbol input =
  let terminal_fn i o = (i, Form.Symbol o) in
    let is_not_symbol_char = (fun c -> not (is_symbol_char c)) in
      read_form input terminal_fn is_not_symbol_char

let try_read input =
  let c = List.hd input in
    if is_digit c then read_num input
    else if is_string_delim c then read_string input
    else if is_symbol_starting_char c then read_symbol input
    else raise (SyntaxError ("unrecognized form '" ^ (implode input) ^ "'"))

let read_list input =
  let rec read_list' input forms =
    match input with
    | [] -> List.rev forms
    | x :: xs when is_blank x -> read_list' xs forms
    | xs -> let (new_input, new_form) = try_read input in
      read_list' new_input (new_form :: forms) in
  read_list' input []

let read s = read_list @@ explode s
