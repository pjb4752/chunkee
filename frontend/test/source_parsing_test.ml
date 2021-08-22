open Frontend.Source_parsing
open OUnit2

module Compile_error = Frontend.Compile_error
module Positional_stream = Frontend.Positional_stream
module Source_form = Frontend.Source_form
module Token_stream = Frontend.Token_stream

module Line_stream = struct
  type lines_t = (string list) ref
  type t = { remaining_lines: lines_t }

  let of_string str =
    let string_lines = String.split_on_char '\n' str in
    { remaining_lines = ref string_lines }

  let next line_stream =
    match !(line_stream.remaining_lines) with
    | [] -> None
    | x :: xs -> let () = line_stream.remaining_lines := xs in Some x
end

let parse_string str =
  let next_string_line line_stream previous_line_number =
    let line_number = previous_line_number + 1 in
    match Line_stream.next line_stream with
    | None-> None
    | Some line -> Some (Positional_stream.of_string ~line_number:line_number line)
  in
  let next_string_line = next_string_line (Line_stream.of_string str) in
  match next_string_line 0 with
  | None -> assert false
  | Some positional_stream ->
  let token_stream = Token_stream.create ~input_callback:next_string_line positional_stream in
  parse_incremental token_stream

let assert_parses_to expected str =
  assert_equal ~printer:Result.inspect (Ok expected) (parse_string str)

let assert_parses_with_error expected str =
  assert_equal ~printer:Result.inspect (Error expected) (parse_string str)

let suite =
  "Source parsing suite">::: [
    "parse single number form">::
      (fun _ ->
        assert_parses_to [Numbers.source_form] Numbers.source_string
      );

    "parse single number form with whitespace">::
      (fun _ ->
        let source_form = Source_form.create_number { line_number = 1; char_number = 3 } 55.0 in
        assert_parses_to [source_form] "\t 55 \t"
      );

    "parse single string form">::
      (fun _ ->
        assert_parses_to [Strings.source_form] Strings.source_string
      );

    "parse single string form with spaces">::
      (fun _ ->
        let source_form = Source_form.create_string { line_number = 1; char_number = 3 } "hello" in
        assert_parses_to [source_form] "\t \"hello\" \t"
      );

    "parse unterminated string">::
      (fun _ ->
        let message = "unexpected end of string" in
        let error = Compile_error.create_syntax_error { line_number = 1; char_number = 6 } message in
        assert_parses_with_error error "\"hello"
      );

    "parse symbol form">::
      (fun _ ->
        let source_form = Source_form.create_symbol { line_number = 1; char_number = 1 } "fat?" in
        assert_parses_to [source_form] "fat?"
      );

    "parse symbol form with spaces">::
      (fun _ ->
        let source_form = Source_form.create_symbol { line_number = 1; char_number = 3 } "fat?" in
        assert_parses_to [source_form] "\t fat? \t"
      );

    "parse single list form">::
      (fun _ ->
        let source_form = Source_form.create_list { line_number = 1; char_number = 1 } [
          Source_form.create_symbol { line_number = 1; char_number = 2 } "+";
          Source_form.create_number { line_number = 1; char_number = 4 } 1.0;
          Source_form.create_number { line_number = 1; char_number = 6 } 2.0
        ]
        in
        assert_parses_to [source_form] "(+ 1 2)"
      );

    "parse list form with extra spaces">::
      (fun _ ->
        let source_form = Source_form.create_list { line_number = 1; char_number = 3 } [
          Source_form.create_symbol { line_number = 1; char_number = 7 } "+";
          Source_form.create_number { line_number = 1; char_number = 9 } 1.0;
          Source_form.create_number { line_number = 2; char_number = 3 } 2.0
        ]
        in
        assert_parses_to [source_form] "  (   + 1 \n  2   )  "
      );

    "parse unterminated string">::
      (fun _ ->
        let message = "unexpected end of input" in
        let error = Compile_error.create_syntax_error { line_number = 1; char_number = 6 } message in
        assert_parses_with_error error "(+ 1 2"
      );

    "parse def expression">::
      (fun _ ->
        assert_parses_to [Defs.source_form] Defs.source_string
      );

    "parse let expression">::
      (fun _ ->
        assert_parses_to [Lets.source_form] Lets.source_string
      );

    "parse if expression">::
      (fun _ ->
        assert_parses_to [Ifs.source_form] Ifs.source_string
      );

    "parse fn expression">::
      (fun _ ->
        assert_parses_to [Fns.source_form] Fns.source_string
      );
  ]
