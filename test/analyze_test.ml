open Lex
open OUnit2

let module_name = Module.Name.from_string "__test__"
let modul = Module.make module_name

let assert_true value = assert_equal value true

let suite =
  "Analyze suite">::: [
    "analyze number literal">::
      (fun context ->
        assert_equal
          (Analyze.analyze (Form.Number 55.0))
          (Ok (Node.NumLit 55.0))
        );

    "analyze string literal">::
      (fun context ->
        assert_equal
          (Analyze.analyze (Form.String "hello cat"))
          (Ok (Node.StrLit "hello cat"))
        );

    "analyze symbol literal">::
      (fun context ->
        assert_equal
          (Analyze.analyze (Form.Symbol "foofoo"))
          (Ok (Node.SymLit "foofoo"))
        );

    "analyze valid def form">::
      (fun context ->
        let form = Form.List [
          Form.Symbol "def"; Form.Symbol "x"; Form.Number 55.0
        ] in
        assert_equal
          (Analyze.analyze form)
          (Ok (Node.Def (Module.Var.Name.from_string "x", Node.NumLit 55.0)))
        );

    "analyze invalid def form">::
      (fun context ->
        let form = Form.List [
          Form.Symbol "def"; Form.Symbol "x";
        ] in
        assert_true (Result.is_error (Analyze.analyze form))
        );

    "analyze valid fn form">::
      (fun context ->
        let form = Form.List [
          Form.Symbol "fn"; Form.List [Form.Symbol "a"]; Form.Symbol "a"
        ] in
        assert_equal
          (Analyze.analyze form)
          (Ok (Node.Fn ([Node.Param.from_string "a"], Node.SymLit "a")))
      );

    "analyze invalid fn form">::
      (fun context ->
        let form = Form.List [
          Form.Symbol "fn"; Form.Symbol "a"
        ] in
        assert_true (Result.is_error (Analyze.analyze form))
      );

    "analyze valid if form">::
      (fun context ->
        let form = Form.List [
          Form.Symbol "if";
            Form.Symbol "a"; Form.Symbol "b"; Form.Symbol "c"
        ] in
        assert_equal
          (Analyze.analyze form)
          (Ok (Node.If (Node.SymLit "a", Node.SymLit "b", Node.SymLit "c")))
      );

    "analyze invalid if form">::
      (fun context ->
        let form = Form.List [
          Form.Symbol "if";
            Form.Symbol "a"; Form.Symbol "b";
        ] in
        assert_true (Result.is_error (Analyze.analyze form))
      );

    "analyze valid let form">::
      (fun context ->
        let form = Form.List [
          Form.Symbol "let";
            Form.List [Form.Symbol "a"; Form.Number 5.0];
            Form.Symbol "a";
        ] in
        let name = Node.Binding.Name.from_string "a" in
        let binding = Node.Binding.from_node name (Node.NumLit 5.0) in
        assert_equal
          (Analyze.analyze form)
          (Ok (Node.Let ([binding], (Node.SymLit "a"))))
      );

    "analyze invalid let form">::
      (fun context ->
        let form = Form.List [
          Form.Symbol "let";
            Form.List [Form.Symbol "a"];
            Form.Symbol "a";
        ] in
        assert_true (Result.is_error (Analyze.analyze form))
      );

    "analyze valid apply form">::
      (fun context ->
        let form = Form.List [
          Form.Symbol "+"; Form.Symbol "a"; Form.Symbol "b";
        ] in
        assert_equal
          (Analyze.analyze form)
          (Ok (Node.Apply (Node.SymLit "+",
            [Node.SymLit "a"; Node.SymLit "b"])))
      );
  ]
