open Frontend
open Frontend.Lex
open Frontend.Metadata
open Frontend.Parse
open OUnit2

let assert_parses_to expected tokens =
  assert_equal ~printer:Result.inspect (Ok expected) (parse_node tokens)

let assert_true value = assert_equal value true

let suite =
  "Parse suite">::: [
    "parse simple def">::
      (fun _ ->
        let init_metadata = { line_num = 1; char_num = 1 } in
        let expr_metadata = { line_num = 1; char_num = 6 } in
        let def = Form.Symbol (init_metadata, "def", "def") in
        let variable = Form.Symbol ({ line_num = 1; char_num = 4 }, "x", "x") in
        let expression = Form.String (expr_metadata, "hello", "hello") in
        let def_form = Form.List (init_metadata, "(def x \"hello\")", [def; variable; expression]) in
        let name = Identifier.from_string "x" in
        let expression_node = Node.StrLit ("hello", expr_metadata) in
        assert_parses_to (Node.Def (name, expression_node, init_metadata)) def_form
      );
  ]
