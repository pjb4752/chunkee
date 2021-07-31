open Printf

module StdString = String

type stream_t = Positional_stream.t
type t = {
  current_stream: stream_t ref;
  next_stream_thunk: (int -> stream_t option)
}

let raise_error position message =
  raise (Token.Error (position, message))

module Whitespace = struct
  let chars = [' '; '\t'; '\n']

  let is_char c = List.mem c chars
end

module LeftParen = struct
  let start_char = '('

  let is_start_char c = c = start_char

  let tokenize input_stream =
    let _ = Positional_stream.next input_stream
    in Token.Marker LeftParen
end

module RightParen = struct
  let start_char = ')'

  let is_start_char c = c = start_char

  let tokenize input_stream =
    let _ = Positional_stream.next input_stream
    in Token.Marker RightParen
end

module Delimiters = struct
  let closing = [
    RightParen.start_char;
  ]

  let is_closing_char c = List.mem c closing
end

module Number = struct
  let decimal_char = '.'

  let digit_chars = ['0'; '1'; '2'; '3'; '4'; '5'; '6'; '7'; '8'; '9']

  (*let sign_chars = ['+'; '-']*)

  let start_chars = digit_chars

  let chars = digit_chars @ [decimal_char]

  let is_decimal_char c = decimal_char = c

  let is_start_char c = List.mem c start_chars

  let is_final_char c = Delimiters.is_closing_char c

  let is_char c = List.mem c chars

  let tokenize input_stream =
    let rec tokenize' ?decimal_found:(decimal_found=false) characters =
      let (position, character) = Positional_stream.peek input_stream in
      match character with
      | None -> characters
      | Some c when Whitespace.is_char c || is_final_char c -> characters
      | Some c when is_char c && is_decimal_char c ->
          if decimal_found then raise_error position (sprintf "unexpected character %c in number" c)
          else tokenize' ~decimal_found:true (Positional_stream.next_only input_stream :: characters)
      | Some c when is_char c ->
          tokenize' ~decimal_found:decimal_found (Positional_stream.next_only input_stream :: characters)
      | Some c -> raise_error position (sprintf "unexpected character %c in number" c)
    in
    let characters = tokenize' [] in
    let characters = List.rev characters in
    let token_text = StdString.of_seq (List.to_seq characters) in
    Token.Atom (Number (float_of_string token_text))
end

module String = struct
  let quote_char = '"'

  let is_quote_char c = c = quote_char

  let is_start_char = is_quote_char

  let tokenize input_stream =
    let () = Positional_stream.junk input_stream in
    let rec tokenize' characters =
      let (position, character) = Positional_stream.peek input_stream in
      match character with
      | None -> raise_error position "unexpected end of string"
      | Some c when is_quote_char c -> let () = Positional_stream.junk input_stream in characters
      | Some c -> let () = Positional_stream.junk input_stream in tokenize' (c :: characters)
    in
    let characters = tokenize' [] in
    let characters = List.rev characters in
    let token_text = StdString.of_seq (List.to_seq characters) in
    Token.Atom (String token_text)
end

module Symbol = struct
  let lower_case_chars = ['a'; 'b'; 'c'; 'd'; 'e'; 'f'; 'g'; 'h'; 'i'; 'j'; 'k'; 'l';
    'm'; 'n'; 'o'; 'p'; 'q'; 'r'; 's'; 't'; 'u'; 'v'; 'w'; 'x'; 'y'; 'z']

  let upper_case_chars = ['A'; 'B'; 'C'; 'D'; 'E'; 'F'; 'G'; 'H'; 'I'; 'J'; 'K'; 'L';
    'M'; 'N'; 'O'; 'P'; 'Q'; 'R'; 'S'; 'T'; 'U'; 'V'; 'W'; 'X'; 'Y'; 'Z']

  let digit_chars = ['0'; '1'; '2'; '3'; '4'; '5'; '6'; '7'; '8'; '9']

  let operator_chars = ['+'; '-'; '*'; '/'; '>'; '<'; '=']

  let punctuation_chars = ['!'; '?'; '.'; ':']

  let start_chars = '_' :: (lower_case_chars @ upper_case_chars @ operator_chars)

  let chars = start_chars @ digit_chars @ punctuation_chars

  let is_start_char c = List.mem c start_chars

  let is_char c = List.mem c chars

  let is_final_char c = Delimiters.is_closing_char c

  let tokenize input_stream =
    let rec tokenize' characters =
      let (position, character) = Positional_stream.peek input_stream in
      match character with
      | None -> characters
      | Some c when Whitespace.is_char c || is_final_char c -> characters
      | Some c when is_char c -> tokenize' (Positional_stream.next_only input_stream :: characters)
      | Some c -> raise_error position (sprintf "unexpected character %c in symbol" c)
    in
    let characters = tokenize' [] in
    let characters = List.rev characters in
    let token_text = StdString.of_seq (List.to_seq characters) in
    Token.Atom (Symbol token_text)
end

let token_for_char input_stream position = function
  | c when Number.is_start_char c -> Number.tokenize input_stream
  | c when String.is_start_char c -> String.tokenize input_stream
  | c when Symbol.is_start_char c -> Symbol.tokenize input_stream
  | c when LeftParen.is_start_char c -> LeftParen.tokenize input_stream
  | c when RightParen.is_start_char c -> RightParen.tokenize input_stream
  | c -> raise_error position (sprintf "invalid token at %c" c)

let create ?input_callback:(input_callback=(fun _ -> None)) current_stream =
  { current_stream = ref current_stream; next_stream_thunk = input_callback }

let next ?inside_form:(inside_form=true) token_stream =
  let token_from_value position value = { Token.position; value } in
  let end_of_input_token position = token_from_value position Token.EndOfInput in
  let rec tokenize' () =
    let input_stream = !(token_stream.current_stream) in
    let (position, character) = Positional_stream.peek input_stream in
    match character with
    | None when inside_form -> begin
      match token_stream.next_stream_thunk position.line_number with
      | None -> end_of_input_token position
      | Some stream -> (token_stream.current_stream := stream) |> tokenize'
    end
    | None -> end_of_input_token position
    | Some c when Whitespace.is_char c -> Positional_stream.junk input_stream |> tokenize'
    | Some c -> token_from_value position @@ token_for_char input_stream position c
  in
  tokenize' ()
