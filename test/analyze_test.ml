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

    "analyze valid fn form">::
      (fun context ->
        let form = Form.List [
          (Form.Symbol "fn"); (Form.List [Form.Symbol "a"]); (Form.Symbol "a")
        ] in
        assert_equal
          (Analyze.analyze modul [form])
          [Ok (Node.Fn ([Node.Param.from_string "a"], Node.SymLit "a"))]
      );

    "analyze invalid fn form">::
      (fun context ->
        let form = Form.List [
          (Form.Symbol "fn"); (Form.Symbol "a")
        ] in
        let c_result = (Analyze.analyze modul [form]) in
        assert_equal
          (Result.is_error (List.hd c_result))
          true
      );

    "analyze valid if form">::
      (fun context ->
        let form = Form.List [
          (Form.Symbol "if");
            (Form.Symbol "a"); (Form.Symbol "b"); (Form.Symbol "c")
        ] in
        assert_equal
          (Analyze.analyze modul [form])
          [Ok (Node.If (Node.SymLit "a", Node.SymLit "b", Node.SymLit "c"))]
      );

    "analyze invalid if form">::
      (fun context ->
        let form = Form.List [
          (Form.Symbol "if");
            (Form.Symbol "a"); (Form.Symbol "b");
        ] in
        let c_result = (Analyze.analyze modul [form]) in
        assert_equal
          (Result.is_error (List.hd c_result))
          true
      );

    "analyze valid let form">::
      (fun context ->
        let form = Form.List [
          (Form.Symbol "let");
            (Form.List [Form.Symbol "a"; Form.Number 5.0]);
            (Form.Symbol "a");
        ] in
        let name = Node.Binding.Name.from_string "a" in
        let binding = Node.Binding.from_node name (Node.NumLit 5.0) in
        assert_equal
          (Analyze.analyze modul [form])
          [Ok (Node.Let ([binding], (Node.SymLit "a")))]
      );

    "analyze invalid let form">::
      (fun context ->
        let form = Form.List [
          (Form.Symbol "let");
            (Form.List [Form.Symbol "a"]);
            (Form.Symbol "a");
        ] in
        let c_result = (Analyze.analyze modul [form]) in
        assert_equal
          (Result.is_error (List.hd c_result))
          true
      );
  ]
