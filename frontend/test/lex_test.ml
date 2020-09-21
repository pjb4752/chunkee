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

    "lex complex nested forms">::
      (fun _ ->
        let str = String.concat "\n" [
          "(def do-math (fn [[x num y num] num]";
          "  (let [first x";
          "        second y]";
          "    (+ first (- second 5)))))";
        ] in
        let def = Form.Symbol ({ line_num = 1; char_num = 2 }, "def", "def") in
        let do_math = Form.Symbol ({ line_num = 1; char_num = 6 }, "do-math", "do-math") in
        let fn = Form.Symbol ({ line_num = 1; char_num = 15 }, "fn", "fn") in
        let x_arg = Form.Symbol ({ line_num = 1; char_num = 20 }, "x", "x") in
        let x_num = Form.Symbol ({ line_num = 1; char_num = 22 }, "num", "num") in
        let y_arg = Form.Symbol ({ line_num = 1; char_num = 26 }, "y", "y") in
        let y_num = Form.Symbol ({ line_num = 1; char_num = 28 }, "num", "num") in
        let fn_args = Form.Vector ({ line_num = 1; char_num = 19 }, "[x num y num]",
          [x_arg; x_num; y_arg; y_num]) in
        let fn_ret = Form.Symbol ({ line_num = 1; char_num = 33 }, "num", "num") in
        let fn_spec = Form.Vector ({ line_num = 1; char_num = 18 }, "[[x num y num] num]", [fn_args; fn_ret]) in
        let lets = Form.Symbol ({ line_num = 2; char_num = 4 }, "let", "let") in
        let let_first = Form.Symbol ({ line_num = 2; char_num = 9 }, "first", "first") in
        let let_x = Form.Symbol ({ line_num = 2; char_num = 15 }, "x", "x") in
        let let_second = Form.Symbol ({ line_num = 3; char_num = 9 }, "second", "second") in
        let let_y = Form.Symbol ({ line_num = 3; char_num = 16 }, "y", "y") in
        let let_binds = Form.Vector ({ line_num = 2; char_num = 8 }, "[first x\n        second y]",
          [let_first; let_x; let_second; let_y]) in
        let plus = Form.Symbol ({ line_num = 4; char_num = 6 }, "+", "+") in
        let first = Form.Symbol ({ line_num = 4; char_num = 8 }, "first", "first") in
        let minus = Form.Symbol ({ line_num = 4; char_num = 15 }, "-", "-") in
        let second = Form.Symbol ({ line_num = 4; char_num = 17 }, "second", "second") in
        let five = Form.Number ({ line_num = 4; char_num = 24 }, "5", 5.00) in
        let subtract = Form.List ({ line_num = 4; char_num = 14 }, "(- second 5)", [minus; second; five]) in
        let add = Form.List ({ line_num = 4; char_num = 5 }, "(+ first (- second 5))",
          [plus; first; subtract]) in
        let let_form = Form.List ({ line_num = 2; char_num = 3 },
          "(let [first x\n        second y]\n    (+ first (- second 5)))",
          [lets; let_binds; add]) in
        let fn_form = Form.List({ line_num = 1; char_num = 14 },
          "(fn [[x num y num] num]\n  (let [first x\n        second y]\n    (+ first (- second 5))))",
          [fn; fn_spec; let_form]) in
        assert_lexes_to [Form.List ({ line_num = 1; char_num = 1 },
          "(def do-math (fn [[x num y num] num]\n  (let [first x\n        second y]\n    (+ first (- second 5)))))",
          [def; do_math; fn_form])] str
      );
  ]
