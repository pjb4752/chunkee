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
let is_list_open = is_char_of ['(']
let is_list_close = is_char_of [')']

let string_to_number s = Form.Number (float_of_string s)

let rec remove_blank = function
  | [] -> []
  | x :: xs when not (is_blank x) -> x :: xs
  | _ :: xs -> remove_blank xs

let read_form input terminal_fn test_fn =
  let rec read_form' input output =
    match input with
    | [] -> terminal_fn input output
    | x :: xs when test_fn x -> terminal_fn input output
    | x :: xs -> read_form' xs (append_char output x) in
  read_form' input ""

(* TODO: needs better handling of decimal points, preceding +/- *)
let read_num input =
  let terminal_fn i o = (i, string_to_number o)
  and is_not_digit = (fun c -> not (is_digit c)) in
  read_form input terminal_fn is_not_digit

let read_symbol input =
  let terminal_fn i o = (i, Form.Symbol o)
  and is_not_symbol_char = (fun c -> not (is_symbol_char c)) in
  read_form input terminal_fn is_not_symbol_char

let read_delimited input start delimiter terminal_fn input_fn =
  let rec read_delimited' input output =
    match input with
    | [] -> raise (SyntaxError (Printf.sprintf "expecting '%c', none found" delimiter))
    | x :: xs when x = delimiter -> (xs, terminal_fn output)
    | x :: xs ->
        let (new_input, new_output) = input_fn input output in
        read_delimited' new_input new_output in
  read_delimited' input start

let read_string input =
  let terminal_fn = (fun out -> Form.String out)
  and input_fn i out =
    match i with
    | [] -> assert false
    | x :: xs -> (xs, append_char out x) in
  read_delimited (List.tl input) "" '"' terminal_fn input_fn

let read_list read_fn input =
  let terminal_fn = (fun out -> Form.List (List.rev out))
  and input_fn = (fun i out ->
    match i with
    | [] -> assert false
    | x :: xs when (is_blank x) -> let new_i = remove_blank i in (new_i, out)
    | x :: xs -> let (new_i, f) = read_fn i in (new_i, f :: out)) in
  read_delimited (List.tl input) [] ')' terminal_fn input_fn

let rec try_read input =
  let c = List.hd input in
  if is_digit c then read_num input
  else if is_string_delim c then read_string input
  else if is_symbol_starting_char c then read_symbol input
  else if is_list_open c then read_list try_read input
  else raise (SyntaxError ("unrecognized form '" ^ (implode input) ^ "'"))

let read_forms input =
  let rec read_list' input forms =
    match input with
    | [] -> List.rev forms
    | x :: xs when is_blank x -> read_list' xs forms
    | xs ->
        let (new_input, new_form) = try_read input in
        read_list' new_input (new_form :: forms) in
  read_list' input []

let read s = read_forms @@ explode s
