open Emit
open OUnit2
open Resolve

let state = State.make ()
let emit = emit_node state

let suite =
  "Emit suite">::: [
    "emit number literal">::
      (fun context ->
        assert_equal
          (emit (Node.NumLit 55.0))
          "55.000000"
      );

    "emit string literal">::
      (fun context ->
        assert_equal
          (emit (Node.StrLit "hello"))
          "\"hello\""
      );

    "emit symbol literal">::
      (fun context ->
        assert_equal
          (emit (Node.SymLit (Name.Local "a")))
          "a"
      );

    "emit def expression">::
      (fun context ->
        let name = Module.Var.Name.from_string "hi"
        and expr = Node.NumLit 5.0 in
        assert_equal
          (emit (Node.Def (name, expr)))
          "local hi = 5.000000"
      );

    "emit fn expression">::
      (fun context ->
        let param0 = Node.Param.from_string "p1" in
        let params = [param0]
        and body = Node.SymLit (Name.Local "p1") in
        assert_equal
          (emit (Node.Fn (params, body)))
          (String.concat "\n" [
            "function(p1)";
            "local __var1";
            "__var1 = p1";
            "return __var1";
            "end";
          ])
      );

    "emit if expression">::
      (fun context ->
        let tst = Node.SymLit (Name.Local "is_true")
        and iff = Node.SymLit (Name.Local "a")
        and els = Node.SymLit (Name.Local "b") in
        assert_equal
          (emit (Node.If (tst, iff, els)))
          (String.concat "\n" [
            "local __var1";
            "if is_true then";
            "__var1 = a";
            "else";
            "__var1 = b";
            "end";
          ])
      );

    "emit if expression with complex test expression">::
      (fun context ->
        let outer_tst = Node.SymLit (Name.Local "is_true")
        and outer_iff = Node.StrLit "hi"
        and outer_els = Node.StrLit "bye" in
        let tst = Node.If (outer_tst, outer_iff, outer_els)
        and iff = Node.SymLit (Name.Local "a")
        and els = Node.SymLit (Name.Local "b") in
        assert_equal
          (emit (Node.If (tst, iff, els)))
          (String.concat "\n" [
            "local __var1";
            "local __var2";
            "if is_true then";
            "__var2 = \"hi\"";
            "else";
            "__var2 = \"bye\"";
            "end";
            "if __var2 then";
            "__var1 = a";
            "else";
            "__var1 = b";
            "end";
          ])
      );

    "emit let expression">::
      (fun context ->
        let name = Node.Binding.Name.from_string "b1"
        and expr = Node.SymLit (Name.Local "a") in
        let bindings = [Node.Binding.from_node name expr]
        and body = Node.SymLit (Name.Local "b1") in
        assert_equal
          (emit (Node.Let (bindings, body)))
          (String.concat "\n" [
            "local __var1";
            "do";
            "local b1 = a";
            "__var1 = b1";
            "end";
          ])
      );

    "emit apply expression">::
      (fun context ->
        let fn = Node.SymLit (Name.Local "a")
        and args = [Node.NumLit 1.0; Node.StrLit "hi"] in
        assert_equal
          (emit (Node.Apply (fn, args)))
          "a(1.000000, \"hi\")"
      );

    "emit apply expression with complex arguments">::
      (fun context ->
        let tst = Node.SymLit (Name.Local "is_true")
        and iff = Node.SymLit (Name.Local "a")
        and els = Node.SymLit (Name.Local "b")
        and fn = Node.SymLit (Name.Local "a") in
        let args = [Node.If (tst, iff, els); Node.StrLit "hi"] in
        assert_equal
          (emit (Node.Apply (fn, args)))
          (String.concat "\n" [
            "local __var1";
            "if is_true then";
            "__var1 = a";
            "else";
            "__var1 = b";
            "end";
            "a(__var1, \"hi\")";
          ])
      );
  ]
