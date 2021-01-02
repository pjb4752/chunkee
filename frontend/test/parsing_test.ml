open Frontend.Parsing
open OUnit2

let assert_parses_to expected tokens =
  assert_equal ~printer:Result.inspect (Ok expected) (parse_node ~check_toplevel:false tokens)

let assert_true value = assert_equal value true

let suite =
  "Parsing suite">::: [
    "parse number form">::
      (fun _ ->
        assert_parses_to Numbers.parsed_value Numbers.lexed_value
      );

    "parse string form">::
      (fun _ ->
        assert_parses_to Strings.parsed_value Strings.lexed_value
      );

    "parse symbol form">::
      (fun _ ->
        assert_parses_to Symbols.parsed_value Symbols.lexed_value
      );

    "parse def expression">::
      (fun _ ->
        assert_parses_to Defs.parsed_value Defs.lexed_value
      );

    "parse let expression">::
      (fun _ ->
        assert_parses_to Lets.parsed_value Lets.lexed_value
      );

    "parse if expression">::
      (fun _ ->
        assert_parses_to Ifs.parsed_value Ifs.lexed_value
      );

    "parse fn expression">::
      (fun _ ->
        assert_parses_to Fns.parsed_value Fns.lexed_value
      );
  ]
