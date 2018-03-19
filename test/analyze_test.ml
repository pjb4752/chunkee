open OUnit2

let module_name = Module.Name.from_string "__test__"
let modul = Module.make module_name

let suite =
  "Analyze suite">::: [
    "analyze number literal">::
      (fun context ->
        assert_equal
          (Analyze.analyze modul [Form.Number 55.0])
          [Ok (Node.NumLit 55.0)]
        );

    "analyze string literal">::
      (fun context ->
        assert_equal
          (Analyze.analyze modul [Form.String "hello cat"])
          [Ok (Node.StrLit "hello cat")]
        );

    "analyze symbol literal">::
      (fun context ->
        assert_equal
          (Analyze.analyze modul [Form.Symbol "foofoo"])
          [Ok (Node.SymLit "foofoo")]
        );

    "analyze valid def form">::
      (fun context ->
        let form = Form.List [
          (Form.Symbol "def"); (Form.Symbol "x"); (Form.Number 55.0)
        ] in
        assert_equal
          (Analyze.analyze modul [form])
          [Ok (Node.Def (Module.Var.Name.from_string "x", Node.NumLit 55.0))]
        );

    "analyze invalid def form">::
      (fun context ->
        let form = Form.List [
          (Form.Symbol "def"); (Form.Symbol "x");
        ] in
        let c_result = (Analyze.analyze modul [form]) in
        assert_equal
          (Result.is_error (List.hd c_result))
          true
        );
  ]
