open Chunkee.Lex
open OUnit2

let assert_lexes_to expected str =
  assert_equal ~printer:debug_string (Ok expected) (lex str)

let assert_lexes_with_error expected str =
  assert_equal ~printer:debug_string (Error expected) (lex str)

let suite =
  "Lex suite">::: [
    "lex single number form">::
      (fun _ ->
        assert_lexes_to [Form.Number (55.0, { line_num = 1; char_num = 1 })] "55"
      );

    "lex single number form with whitespace">::
      (fun _ ->
        assert_lexes_to [Form.Number (55.0, { line_num = 1; char_num = 3 })] "\t 55 \t"
      );

    "lex single string form">::
      (fun _ ->
        assert_lexes_to [Form.String ("hello", { line_num = 1; char_num = 1 })] "\"hello\""
      );

    "lex single string form with spaces">::
      (fun _ ->
        assert_lexes_to [Form.String ("hello", { line_num = 1; char_num = 3 })] "\t \"hello\" \t"
      );

    "lex unterminated string">::
      (fun _ ->
        let prefix = "in expression at 1:1" in
        let message = "unclosed expression found, expecting delimiter '\"'" in
        assert_lexes_with_error (SyntaxError { line_num = 1; char_num = 1; prefix ; message })  "\"hello"
      );

    "lex symbol form">::
      (fun _ ->
        assert_lexes_to [Form.Symbol ("fat?", { line_num = 1; char_num = 1 })] "fat?"
      );

    "lex symbol form with spaces">::
      (fun _ ->
        assert_lexes_to [Form.Symbol ("fat?", { line_num = 1; char_num = 3 })] "\t fat? \t"
      );

    "lex single list form">::
      (fun _ ->
        let plus = Form.Symbol ("+", { line_num = 1; char_num = 2 }) in
        let one = Form.Number (1.0, { line_num = 1; char_num = 4 }) in
        let two = Form.Number (2.0, { line_num = 1; char_num = 6 }) in
        assert_lexes_to [Form.List([plus; one; two], { line_num = 1; char_num = 1 })] "(+ 1 2)"
      );

    "lex list form with extra spaces">::
      (fun _ ->
        let plus = Form.Symbol ("+", { line_num = 1; char_num = 7 }) in
        let one = Form.Number (1.0, { line_num = 1; char_num = 9 }) in
        let two = Form.Number (2.0, { line_num = 1; char_num = 13 }) in
        assert_lexes_to [Form.List([plus; one; two], { line_num = 1; char_num = 3 })] "  (   + 1   2   )  "
      );

    "lex unterminated string">::
      (fun _ ->
        let prefix = "in expression at 1:1" in
        let message = "unclosed expression found, expecting delimiter ')'" in
        assert_lexes_with_error (SyntaxError { line_num = 1; char_num = 1; prefix ; message })  "(+ 1 2"
      );

    "lex complex nested forms">::
      (fun _ ->
        let str = String.concat "\n" [
          "(def do-math (fn [[x num y num] num]";
            "(let [first x";
            "      second y]";
              "(+ first (- second 5)))))";
        ] in
        let def = Form.Symbol ("def", { line_num = 1; char_num = 2 }) in
        let do_math = Form.Symbol ("do-math", { line_num = 1; char_num = 6 }) in
        let fn = Form.Symbol ("fn", { line_num = 1; char_num = 15 }) in
        let x_arg = Form.Symbol ("x", { line_num = 1; char_num = 20 }) in
        let x_num = Form.Symbol ("num", { line_num = 1; char_num = 22 }) in
        let y_arg = Form.Symbol ("y", { line_num = 1; char_num = 26 }) in
        let y_num = Form.Symbol ("num", { line_num = 1; char_num = 28 }) in
        let fn_args = Form.Vec ([x_arg; x_num; y_arg; y_num], { line_num = 1; char_num = 19 }) in
        let fn_ret = Form.Symbol ("num", { line_num = 1; char_num = 33 }) in
        let fn_spec = Form.Vec ([fn_args; fn_ret], { line_num = 1; char_num = 18 }) in
        let lets = Form.Symbol ("let", { line_num = 2; char_num = 2 }) in
        let let_first = Form.Symbol ("first", { line_num = 2; char_num = 7 }) in
        let let_x = Form.Symbol ("x", { line_num = 2; char_num = 13 }) in
        let let_second = Form.Symbol ("second", { line_num = 3; char_num = 7 }) in
        let let_y = Form.Symbol ("y", { line_num = 3; char_num = 14 }) in
        let let_binds = Form.Vec ([let_first; let_x; let_second; let_y], { line_num = 2; char_num = 6 }) in
        let plus = Form.Symbol ("+", { line_num = 4; char_num = 2 }) in
        let first = Form.Symbol ("first", { line_num = 4; char_num = 4 }) in
        let minus = Form.Symbol ("-", { line_num = 4; char_num = 11 }) in
        let second = Form.Symbol ("second", { line_num = 4; char_num = 13 }) in
        let five = Form.Number (5.00, { line_num = 4; char_num = 20 }) in
        let subtract = Form.List ([minus; second; five], { line_num = 4; char_num = 10 }) in
        let add = Form.List ([plus; first; subtract], { line_num = 4; char_num = 1 }) in
        let let_form = Form.List ([lets; let_binds; add], { line_num = 2; char_num = 1 }) in
        let fn_form = Form.List([fn; fn_spec; let_form], { line_num = 1; char_num = 14 }) in
        assert_lexes_to [Form.List ([def; do_math; fn_form], { line_num = 1; char_num = 1 })] str
      );
  ]
