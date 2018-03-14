open OUnit2

let suite =
  "Read suite">::: [
    "read number form">::
      (fun context ->
        assert_equal (Read.read "55") [(Form.Number 55.0)]
        );

    "read string form">::
      (fun context ->
        assert_equal (Read.read "\"hello\"") [(Form.String "hello")]
        );

    "read unterminated string form">::
      (fun context ->
        assert_raises
          (Read.SyntaxError "expecting '\"', none found")
          (fun () -> (Read.read "\"hello"))
          );

    "read symbol form">::
      (fun context ->
        assert_equal (Read.read "fat?") [(Form.Symbol "fat?")]
        );
  ]
