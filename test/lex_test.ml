open Chunkee.Lex
open OUnit2

let suite =
  "Lex suite">::: [
    "lex number form">::
      (fun _ ->
        assert_equal (lex_exn "55") [(Form.Number 55.0)]
        );

    "lex string form">::
      (fun _ ->
        assert_equal (lex_exn "\"hello\"") [(Form.String "hello")]
        );

    "lex unterminated string form">::
      (fun _ ->
        assert_raises
          (SyntaxError "expecting '\"', none found")
          (fun () -> (lex_exn "\"hello"))
          );

    "lex symbol form">::
      (fun _ ->
        assert_equal (lex_exn "fat?") [(Form.Symbol "fat?")]
        );

    "lex list form">::
      (fun _ ->
        assert_equal (lex_exn "(+ 1 2)")
        [(Form.List [(Form.Symbol "+"); (Form.Number 1.0); (Form.Number 2.0)])]
        );

    "lex list form with extra spaces">::
      (fun _ ->
        assert_equal (lex_exn "(   + 1   2   )")
        [(Form.List [(Form.Symbol "+"); (Form.Number 1.0); (Form.Number 2.0)])]
        );
  ]
