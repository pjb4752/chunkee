open Frontend.Semantic_parsing
open OUnit2

let assert_parses_to expected tokens =
  assert_equal ~printer:Result.inspect (Ok expected) (parse_form tokens)

let assert_true value = assert_equal value true

let suite =
  "Semantic parsing suite">::: [
    "parse number form">::
      (fun _ ->
        assert_parses_to Numbers.semantic_form Numbers.source_form
      );

    "parse string form">::
      (fun _ ->
        assert_parses_to Strings.semantic_form Strings.source_form
      );

    "parse symbol form">::
      (fun _ ->
        assert_parses_to Symbols.semantic_form Symbols.source_form
      );

    "parse def expression">::
      (fun _ ->
        assert_parses_to Defs.semantic_form Defs.source_form
      );

      (*
    "parse let expression">::
      (fun _ ->
        assert_parses_to Lets.semantic_form Lets.source_form
      );
      *)

    "parse if expression">::
      (fun _ ->
        assert_parses_to Ifs.semantic_form Ifs.source_form
      );

      (*
    "parse fn expression">::
      (fun _ ->
        assert_parses_to Fns.semantic_form Fns.source_form
      );
      *)
  ]
