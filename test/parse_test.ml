open Lex
open Parse
open OUnit2

let assert_true value = assert_equal value true

let suite =
  "Parse suite">::: [
    "parse number literal">::
      (fun context ->
        assert_equal
          (parse_form (Form.Number 55.0))
          (Ok (Node.NumLit 55.0))
        );

    "parse string literal">::
      (fun context ->
        assert_equal
          (parse_form (Form.String "hello cat"))
          (Ok (Node.StrLit "hello cat"))
        );

    "parse symbol literal">::
      (fun context ->
        assert_equal
          (parse_form (Form.Symbol "foofoo"))
          (Ok (Node.SymLit "foofoo"))
        );

    "parse valid def form">::
      (fun context ->
        let form = Form.List [
          Form.Symbol "def";
          Form.Vec [Form.Symbol "x"; Form.Symbol "num"];
          Form.Number 55.0
        ]
        and name = Node.VarDef.Name.from_string "x"
        and t = Node.VarDef.Type.from_string "num" in
        assert_equal
          (parse_form form)
          (Ok (Node.Def (Node.VarDef.from_parts name t, Node.NumLit 55.0)))
        );

    "parse valid def form with fn type">::
      (fun context ->
        let form = Form.List [
          Form.Symbol "def";
          Form.Vec [
            Form.Symbol "x";
            Form.Vec [Form.Symbol "num"; Form.Symbol "str"]
          ];
          Form.Number 55.0
        ]
        and name = Node.VarDef.Name.from_string "x"
        and t0 = Node.VarDef.Type.from_string "num"
        and t1 = Node.VarDef.Type.from_string "str" in
        let t = Node.VarDef.Type.from_list [t0; t1] in
        assert_equal
          (parse_form form)
          (Ok (Node.Def (Node.VarDef.from_parts name t, Node.NumLit 55.0)))
        );

    "parse invalid def form">::
      (fun context ->
        let form = Form.List [
          Form.Symbol "def"; Form.Symbol "x";
        ] in
        assert_true (Result.is_error (parse_form form))
        );

    "parse valid fn form">::
      (fun context ->
        let form = Form.List [
          Form.Symbol "fn";
          Form.Vec [
            Form.Vec [Form.Symbol "a"; Form.Symbol "num"];
          ];
          Form.Symbol "a"
        ]
        and name0 = Node.VarDef.Name.from_string "a"
        and type0 = Node.VarDef.Type.from_string "num" in
        let param0 = Node.VarDef.from_parts name0 type0 in
        assert_equal
          (parse_form form)
          (Ok (Node.Fn ([param0], Node.SymLit "a")))
      );

    "parse invalid fn form">::
      (fun context ->
        let form = Form.List [
          Form.Symbol "fn"; Form.Symbol "a"
        ] in
        assert_true (Result.is_error (parse_form form))
      );

    "parse valid if form">::
      (fun context ->
        let form = Form.List [
          Form.Symbol "if";
            Form.Symbol "a"; Form.Symbol "b"; Form.Symbol "c"
        ] in
        assert_equal
          (parse_form form)
          (Ok (Node.If (Node.SymLit "a", Node.SymLit "b", Node.SymLit "c")))
      );

    "parse invalid if form">::
      (fun context ->
        let form = Form.List [
          Form.Symbol "if";
            Form.Symbol "a"; Form.Symbol "b";
        ] in
        assert_true (Result.is_error (parse_form form))
      );

    "parse valid let form">::
      (fun context ->
        let form = Form.List [
          Form.Symbol "let";
            Form.List [Form.Symbol "a"; Form.Number 5.0];
            Form.Symbol "a";
        ] in
        let name = Node.Binding.Name.from_string "a" in
        let binding = Node.Binding.from_node name (Node.NumLit 5.0) in
        assert_equal
          (parse_form form)
          (Ok (Node.Let ([binding], (Node.SymLit "a"))))
      );

    "parse invalid let form">::
      (fun context ->
        let form = Form.List [
          Form.Symbol "let";
            Form.List [Form.Symbol "a"];
            Form.Symbol "a";
        ] in
        assert_true (Result.is_error (parse_form form))
      );

    "parse valid apply form">::
      (fun context ->
        let form = Form.List [
          Form.Symbol "+"; Form.Symbol "a"; Form.Symbol "b";
        ] in
        assert_equal
          (parse_form form)
          (Ok (Node.Apply (Node.SymLit "+",
            [Node.SymLit "a"; Node.SymLit "b"])))
      );
  ]
