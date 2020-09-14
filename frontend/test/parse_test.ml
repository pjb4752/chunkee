open Chunkee
open Chunkee.Lex
open Chunkee.Metadata
open Chunkee.Parse
open OUnit2

let assert_parses_to expected tokens =
  assert_equal ~printer:inspect (Ok expected) (parse tokens)

let assert_true value = assert_equal value true

let suite =
  "Parse suite">::: [
    "parse simple def">::
      (fun _ ->
        let init_metadata = { line_num = 1; char_num = 1 } in
        let expr_metadata = { line_num = 1; char_num = 6 } in
        let def = Form.Symbol ("def", init_metadata) in
        let variable = Form.Symbol ("x", { line_num = 1; char_num = 4 }) in
        let expression = Form.String ("hello", expr_metadata) in
        let def_form = Form.List ([def; variable; expression], init_metadata) in
        let name = Identifier.from_string "x" in
        let expression_node = Node.StrLit ("hello", expr_metadata) in
        assert_parses_to [Node.Def (name, expression_node, init_metadata)] [def_form]
      );

    "parse defrecord">::
      (fun _ ->
        let init_metadata = { line_num = 1; char_num = 1 } in
        let defrecord = Form.Symbol ("defrec", init_metadata) in
        let record_name = Form.Cons ("TestRecord", { line_num = 1; char_num = 10 }) in
        let name1 = Form.Symbol ("field1", { line_num = 1; char_num = 15 }) in
        let type1 = Form.Symbol ("num", { line_num = 1; char_num = 20 }) in
        let name2 = Form.Symbol ("field2", { line_num = 1; char_num = 15 }) in
        let type2 = Form.Symbol ("str", { line_num = 1; char_num = 20 }) in
        let fields = Form.Vec ([name1; type1; name2; type2], { line_num = 1; char_num = 15 }) in
        let defrecord_form = Form.List ([defrecord; record_name; fields], init_metadata) in

        let record_name = Identifier.from_string "TestRecord" in
        let name1 = Identifier.from_string "field1" in
        let type1 = Type_expr.SimpleType (Name_expr.BareName "num") in
        let name2 = Identifier.from_string "field2" in
        let type2 = Type_expr.SimpleType (Name_expr.BareName "str") in
        let fields = [Node.VarDef.from_parts name1 type1; Node.VarDef.from_parts name2 type2] in
        let expected = Node.Rec (record_name, fields, init_metadata) in
        assert_parses_to [expected] [defrecord_form]
      );
  ]
