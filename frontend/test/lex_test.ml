open Frontend.Lex
open OUnit2

let assert_lexes_to expected str =
  assert_equal ~printer:Result.inspect (Ok expected) (lex str)

let assert_lexes_with_error expected str =
  assert_equal ~printer:Result.inspect (Error expected) (lex str)

let suite =
  "Lex suite">::: [
    "lex single number form">::
      (fun _ ->
        assert_lexes_to [Form.Number ({ line_num = 1; char_num = 1 }, "55", 55.0)] "55"
      );

    "lex single number form with whitespace">::
      (fun _ ->
        assert_lexes_to [Form.Number ({ line_num = 1; char_num = 3 }, "55", 55.0)] "\t 55 \t"
      );

    "lex single string form">::
      (fun _ ->
        assert_lexes_to [Form.String ({ line_num = 1; char_num = 1 }, "\"hello\"", "hello")] "\"hello\""
      );

    "lex single string form with spaces">::
      (fun _ ->
        assert_lexes_to [Form.String ({ line_num = 1; char_num = 3 }, "\"hello\"", "hello")] "\t \"hello\" \t"
      );

    "lex unterminated string">::
      (fun _ ->
        let prefix = "in expression at 1:1" in
        let message = "unclosed expression found, expecting delimiter '\"'" in
        assert_lexes_with_error (SyntaxError { line_num = 1; char_num = 1; prefix ; message })  "\"hello"
      );

    "lex symbol form">::
      (fun _ ->
        assert_lexes_to [Form.Symbol ({ line_num = 1; char_num = 1 }, "fat?", "fat?")] "fat?"
      );

    "lex symbol form with spaces">::
      (fun _ ->
        assert_lexes_to [Form.Symbol ({ line_num = 1; char_num = 3 }, "fat?", "fat?")] "\t fat? \t"
      );

    "lex single list form">::
      (fun _ ->
        let plus = Form.Symbol ({ line_num = 1; char_num = 2 }, "+", "+") in
        let one = Form.Number ({ line_num = 1; char_num = 4 }, "1", 1.0) in
        let two = Form.Number ({ line_num = 1; char_num = 6 }, "2", 2.0) in
        assert_lexes_to [Form.List({ line_num = 1; char_num = 1 }, "(+ 1 2)", [plus; one; two])] "(+ 1 2)"
      );

    "lex list form with extra spaces">::
      (fun _ ->
        let plus = Form.Symbol ({ line_num = 1; char_num = 5 }, "+", "+") in
        let one = Form.Number ({ line_num = 1; char_num = 7 }, "1", 1.0) in
        let two = Form.Number ({ line_num = 2; char_num = 3 }, "2", 2.0) in
        assert_lexes_to [Form.List({ line_num = 1; char_num = 1 }, "(   + 1 \n  2   )", [plus; one; two])]
        "(   + 1 \n  2   )  "
      );

    "lex unterminated string">::
      (fun _ ->
        let prefix = "in expression at 1:1" in
        let message = "unclosed expression found, expecting delimiter ')'" in
        assert_lexes_with_error (SyntaxError { line_num = 1; char_num = 1; prefix ; message })  "(+ 1 2"
      );

    "lex def statement">::
      (fun _ ->
        assert_lexes_to [Lex_def.form] Lex_def.source
      );

    "lex let expression">::
      (fun _ ->
        assert_lexes_to [Lex_let.form] Lex_let.source
      );

    "lex if expression">::
      (fun _ ->
        assert_lexes_to [Lex_if.form] Lex_if.source
      );

    "lex fn expression">::
      (fun _ ->
        assert_lexes_to [Lex_fn.form] Lex_fn.source
      );

    "lex record literal">::
      (fun _ ->
        assert_lexes_to [Lex_record.form] Lex_record.source
      );

    "lex extension expression">::
      (fun _ ->
        assert_lexes_to [Lex_extension.form] Lex_extension.source
      );
  ]
