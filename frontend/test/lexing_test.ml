open Frontend.Lexing
open OUnit2

let assert_lexes_to expected str =
  assert_equal ~printer:Result.inspect (Ok expected) (lex str)

let assert_lexes_with_error expected str =
  assert_equal ~printer:Result.inspect (Error expected) (lex str)

let suite =
  "Lex suite">::: [
    "lex single number form">::
      (fun _ ->
        assert_lexes_to [Numbers.lexed_value] "55"
      );

    "lex single number form with whitespace">::
      (fun _ ->
        let lexed_value = {
          Form.metadata = { line_num = 1; char_num = 3; source = "55" };
          lexed = Form.Number 55.0
        } in
        assert_lexes_to [lexed_value] "\t 55 \t"
      );

    "lex single string form">::
      (fun _ ->
        assert_lexes_to [Strings.lexed_value] "\"hello\""
      );

    "lex single string form with spaces">::
      (fun _ ->
        let lexed_value = {
          Form.metadata = { line_num = 1; char_num = 3; source = "\"hello\"" };
          lexed = Form.String "hello"
        } in
        assert_lexes_to [lexed_value] "\t \"hello\" \t"
      );

    "lex unterminated string">::
      (fun _ ->
        let prefix = "in expression at 1:1" in
        let message = "unclosed expression found, expecting delimiter '\"'" in
        assert_lexes_with_error (SyntaxError { line_num = 1; char_num = 1; prefix ; message })  "\"hello"
      );

    "lex symbol form">::
      (fun _ ->
        let lexed_value = {
          Form.metadata = { line_num = 1; char_num = 1; source = "fat?" };
          lexed = Form.Symbol "fat?"
        } in
        assert_lexes_to [lexed_value] "fat?"
      );

    "lex symbol form with spaces">::
      (fun _ ->
        let lexed_value = {
          Form.metadata = { line_num = 1; char_num = 3; source = "fat?" };
          lexed = Form.Symbol "fat?"
        } in
        assert_lexes_to [lexed_value] "\t fat? \t"
      );

    "lex single list form">::
      (fun _ ->
        let lexed_value = {
          Form.metadata = { line_num = 1; char_num = 1; source = "(+ 1 2)" };
          lexed = Form.List [
            {
              metadata = { line_num = 1; char_num = 2; source = "+" };
              lexed = Form.Symbol "+"
            };
            {
              metadata = { line_num = 1; char_num = 4; source = "1" };
              lexed = Form.Number 1.0
            };
            {
              metadata = { line_num = 1; char_num = 6; source = "2" };
              lexed = Form.Number 2.0
            }
          ]
        } in
        assert_lexes_to [lexed_value] "(+ 1 2)"
      );

    "lex list form with extra spaces">::
      (fun _ ->
        let lexed_value = {
          Form.metadata = { line_num = 1; char_num = 1; source = "(   + 1 \n  2   )" };
          lexed = Form.List [
            {
              metadata = { line_num = 1; char_num = 5; source = "+" };
              lexed = Form.Symbol "+"
            };
            {
              metadata = { line_num = 1; char_num = 7; source = "1" };
              lexed = Form.Number 1.0
            };
            {
              metadata = { line_num = 2; char_num = 3; source = "2" };
              lexed = Form.Number 2.0
            }
          ]
        } in
        assert_lexes_to [lexed_value] "(   + 1 \n  2   )  "
      );

    "lex unterminated string">::
      (fun _ ->
        let prefix = "in expression at 1:1" in
        let message = "unclosed expression found, expecting delimiter ')'" in
        assert_lexes_with_error (SyntaxError { line_num = 1; char_num = 1; prefix ; message })  "(+ 1 2"
      );

    "lex def expression">::
      (fun _ ->
        assert_lexes_to [Defs.lexed_value] Defs.source
      );

    "lex let expression">::
      (fun _ ->
        assert_lexes_to [Lets.lexed_value] Lets.source
      );

    "lex if expression">::
      (fun _ ->
        assert_lexes_to [Ifs.lexed_value] Ifs.source
      );

    "lex fn expression">::
      (fun _ ->
        assert_lexes_to [Fns.lexed_value] Fns.source
      );

    "lex extension expression">::
      (fun _ ->
        assert_lexes_to [Extensions.lexed_value] Extensions.source
      );
  ]
