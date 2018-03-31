open OUnit2

let suite =
  "Read suite">::: [
    "read number form">::
      (fun context ->
        assert_equal (Read.read_exn "55") [(Form.Number 55.0)]
        );

    "read string form">::
      (fun context ->
        assert_equal (Read.read_exn "\"hello\"") [(Form.String "hello")]
        );

    "read unterminated string form">::
      (fun context ->
        assert_raises
          (Read.SyntaxError "expecting '\"', none found")
          (fun () -> (Read.read_exn "\"hello"))
          );

    "read symbol form">::
      (fun context ->
        assert_equal (Read.read_exn "fat?") [(Form.Symbol "fat?")]
        );

    "read list form">::
      (fun context ->
        assert_equal (Read.read_exn "(+ 1 2)")
        [(Form.List [(Form.Symbol "+"); (Form.Number 1.0); (Form.Number 2.0)])]
        );

    "read list form with extra spaces">::
      (fun context ->
        assert_equal (Read.read_exn "(   + 1   2   )")
        [(Form.List [(Form.Symbol "+"); (Form.Number 1.0); (Form.Number 2.0)])]
        );

  ]
