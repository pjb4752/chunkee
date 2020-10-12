open Frontend.Parse
open OUnit2

let assert_parses_to expected tokens =
  assert_equal ~printer:Result.inspect (Ok expected) (parse_node ~check_toplevel:false tokens)

let assert_true value = assert_equal value true

let suite =
  "Parse suite">::: [
    "parse number form">::
      (fun _ ->
        assert_parses_to Numbers.parsed Numbers.form
      );

    "parse string form">::
      (fun _ ->
        assert_parses_to Strings.parsed Strings.form
      );

    "parse symbol form">::
      (fun _ ->
        assert_parses_to Symbols.parsed Symbols.form
      );

    "parse def expression">::
      (fun _ ->
        assert_parses_to Defs.parsed Defs.form
      );

    "parse let expression">::
      (fun _ ->
        assert_parses_to Lets.parsed Lets.form
      );

    "parse if expression">::
      (fun _ ->
        assert_parses_to Ifs.parsed Ifs.form
      );

    "parse fn expression">::
      (fun _ ->
        assert_parses_to Fns.parsed Fns.form
      );
  ]
