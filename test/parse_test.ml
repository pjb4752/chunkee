open Chunkee
open Chunkee.Lex
open Chunkee.Parse
open OUnit2
open Test_helper

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
          (Ok (make_bare_sym "foofoo"))
        );

    "parse valid rec form">::
      (fun context ->
        let form = Form.List [
          Form.Symbol "defrec";
          Form.Symbol "hello";
          Form.Vec [Form.Symbol "x"; Form.Symbol "num"];
        ] in
        let name = Node.Name.from_string "hello" in
        let var_def = make_p_var_def "x" "num" in
        assert_equal
          (parse_form form)
          (Ok (Node.Rec (name, [var_def])))
      );

    "parse valid def form">::
      (fun context ->
        let form = Form.List [
          Form.Symbol "def";
          Form.Symbol "x";
          Form.Number 55.0
        ] in
        let name = Node.Name.from_string "x" in
        let node = Node.NumLit 55.0 in
        assert_equal
          (parse_form form)
          (Ok (Node.Def (name, node)))
        );

    "parse invalid def form">::
      (fun context ->
        let form = Form.List [
          Form.Symbol "def"; Form.Symbol "x";
        ] in
        assert_true (Thwack.Result.is_error (parse_form form))
        );

    "parse valid fn form">::
      (fun context ->
        let form = Form.List [
          Form.Symbol "fn";
          Form.Vec [
            Form.Vec [Form.Symbol "a"; Form.Symbol "num"];
          ];
          Form.Symbol "a"
        ] in
        let param0 = make_p_var_def "a" "num" in
        assert_equal
          (parse_form form)
          (Ok (Node.Fn ([param0], make_bare_sym "a")))
      );

    "parse invalid fn form">::
      (fun context ->
        let form = Form.List [
          Form.Symbol "fn"; Form.Symbol "a"
        ] in
        assert_true (Thwack.Result.is_error (parse_form form))
      );

    "parse valid if form">::
      (fun context ->
        let form = Form.List [
          Form.Symbol "if";
            Form.Symbol "a"; Form.Symbol "b"; Form.Symbol "c"
        ] in
        let tst = make_bare_sym "a" in
        let iff = make_bare_sym "b" in
        let els = make_bare_sym "c" in
        assert_equal
          (parse_form form)
          (Ok (Node.If (tst, iff, els)))
      );

    "parse invalid if form">::
      (fun context ->
        let form = Form.List [
          Form.Symbol "if";
            Form.Symbol "a"; Form.Symbol "b";
        ] in
        assert_true (Thwack.Result.is_error (parse_form form))
      );

    "parse valid let form">::
      (fun context ->
        let form = Form.List [
          Form.Symbol "let";
            Form.Vec [Form.Symbol "a"; Form.Number 5.0];
            Form.Symbol "a";
        ] in
        let name = Node.Binding.Name.from_string "a" in
        let binding = Node.Binding.from_node name (Node.NumLit 5.0) in
        assert_equal
          (parse_form form)
          (Ok (Node.Let ([binding], (make_bare_sym "a"))))
      );

    "parse invalid let form">::
      (fun context ->
        let form = Form.List [
          Form.Symbol "let";
            Form.List [Form.Symbol "a"];
            Form.Symbol "a";
        ] in
        assert_true (Thwack.Result.is_error (parse_form form))
      );

    "parse valid apply form">::
      (fun context ->
        let form = Form.List [
          Form.Symbol "+"; Form.Symbol "a"; Form.Symbol "b";
        ] in
        assert_equal
          (parse_form form)
          (Ok (Node.Apply (make_bare_sym "+",
            [make_bare_sym "a"; make_bare_sym "b"])))
      );

    "parse apply form of anonymous function">::
      (fun context ->
        let fn = Form.List [
          Form.Symbol "fn";
          Form.Vec [
            Form.Vec [Form.Symbol "a"; Form.Symbol "num"];
          ];
          Form.Symbol "a"
        ] in
        let form = Form.List [fn; Form.Number 5.0] in
        let param0 = make_p_var_def "a" "num" in
        let fn_node = Node.Fn ([param0], make_bare_sym "a") in
        assert_equal
          (parse_form form)
          (Ok (Node.Apply (fn_node, [Node.NumLit 5.0])))
      );

    "parse valid cast form">::
      (fun context ->
        let form = Form.List [
          Form.Symbol "cast"; Form.Symbol "num"; Form.Symbol "a"
        ] in
        assert_equal
          (parse_form form)
          (Ok (Node.Cast (make_type_expr "num" , make_bare_sym "a")))
      );
  ]
