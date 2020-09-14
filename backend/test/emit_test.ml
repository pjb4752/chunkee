open Kludge.Emit
open OUnit2

module C = Chunkee
module Lua_stmt = Kludge.Lua_stmt
module Stdlib = Kludge.Stdlib

let pervasive_module = Stdlib.pervasive.modul

let pervasive_name = C.Module.qual_name pervasive_module

let suite =
  "Emit suite">::: [
    "emit number literal">::
      (fun context ->
        assert_equal
          (emit_node (C.Node.NumLit 55.0))
          (Lua_stmt.make_expr "55.000000")
      );

    "emit string literal">::
      (fun context ->
        assert_equal
          (emit_node (C.Node.StrLit "hello"))
          (Lua_stmt.make_expr "\"hello\"")
      );

    "emit symbol literal">::
      (fun context ->
        assert_equal
          (emit_node (C.Node.SymLit (C.Name.Local "a")))
          (Lua_stmt.make_expr "a")
      );

    "emit fn literal">::
      (fun context ->
        let name0 = C.Node.VarDef.Name.from_string "p1" in
        let type0 = C.Node.TypeDef.from_string "num" in
        let param0 = C.Node.VarDef.from_parts name0 type0 in
        let params = [param0] and body = C.Node.SymLit (C.Name.Local "p1") in
        assert_equal
          (emit_node (C.Node.Fn (params, body)))
          (Lua_stmt.make_expr "function(p1)\nreturn p1\nend")
      );

    "emit def statement">::
      (fun context ->
        let name = C.Node.VarDef.Name.from_string "hi"
        and t = C.Node.TypeDef.from_string "num" in
        let var = C.Node.VarDef.from_parts name t in
        let expr = C.Node.NumLit 5.0 in
        let lua_stmt = Lua_stmt.make_stmt "hi" "hi = 5.000000" in
        let preamble = Lua_stmt.make_expr "5.000000" in
        assert_equal
          (emit_node (C.Node.Def (var, expr)))
          (Lua_stmt.insert_preamble lua_stmt preamble)
      );

    "emit def statement with wrapped operator">::
      (fun context ->
        let var_name = C.Node.VarDef.Name.from_string "hi" in
        let t = C.Node.TypeDef.from_string "num" in
        let t = C.Node.TypeDef.from_list [t; t; t] in
        let var = C.Node.VarDef.from_parts var_name t in
        let op_name = C.Module.Var.Name.from_string "+" in
        let mod_name = pervasive_name in
        let expr = C.Node.SymLit (C.Name.Module (mod_name, op_name)) in
        let lua_stmt = Lua_stmt.make_stmt "hi" "hi = core_common.add" in
        let preamble = Lua_stmt.make_expr "+" in
        assert_equal
          (emit_node (C.Node.Def (var, expr)))
          (Lua_stmt.insert_preamble lua_stmt preamble)
      );

    "emit if expression">::
      (fun context ->
        let name = C.Module.Var.Name.from_string "true" in
        let modname = pervasive_name in
        let tst = C.Node.SymLit (C.Name.Module (modname, name)) in
        let iff = C.Node.SymLit (C.Name.Local "a") in
        let els = C.Node.SymLit (C.Name.Local "b") in
        let tst_str = "__if1 = nil\nif true then\n" in
        let body_str = "__if1 = a\nelse\n__if1 = b\nend" in
        let lua_stmt = Lua_stmt.make_stmt "__if1" (tst_str ^ body_str) in
        let preamble = Lua_stmt.make_expr "true" in
        assert_equal
          (emit_node (C.Node.If (tst, iff, els)))
          (Lua_stmt.insert_preamble lua_stmt preamble)
      );

    "emit let expresssion">::
      (fun context ->
        let name = C.Node.Binding.Name.from_string "b1" in
        let expr = C.Node.SymLit (C.Name.Local "a") in
        let bindings = [C.Node.Binding.from_node name expr] in
        let body = C.Node.SymLit (C.Name.Local "b1") in
        let let_str = "__let1 = nil\ndo\nb1 = a\n__let1 = b1\nend" in
        assert_equal
          (emit_node (C.Node.Let (bindings, body)))
          (Lua_stmt.make_stmt "__let1" let_str)
      );

    "emit infix application">::
      (fun context ->
        let name = C.Module.Var.Name.from_string "+" in
        let modname = pervasive_name in
        let fn = C.Node.SymLit (C.Name.Module (modname, name)) in
        let args = [C.Node.NumLit 1.0; C.Node.NumLit 2.0] in
        let lua_stmt = Lua_stmt.make_expr "(1.000000 + 2.000000)" in
        let preamble = Lua_stmt.make_expr "1.000000" in
        let lua_stmt = Lua_stmt.insert_preamble lua_stmt preamble in
        let preamble = Lua_stmt.make_expr "2.000000" in
        assert_equal
          (emit_node (C.Node.Apply (fn, args)))
          (Lua_stmt.insert_preamble lua_stmt preamble)
      );

    "emit prefix application">::
      (fun context ->
        let name = C.Module.Var.Name.from_string "print" in
        let modname = pervasive_name in
        let fn = C.Node.SymLit (C.Name.Module (modname, name)) in
        let args = [C.Node.StrLit "hello"] in
        let lua_stmt = Lua_stmt.make_expr "print(\"hello\")" in
        let preamble = Lua_stmt.make_expr "\"hello\"" in
        assert_equal
          (emit_node (C.Node.Apply (fn, args)))
          (Lua_stmt.insert_preamble lua_stmt preamble)
      );

    "emit apply expression with anonymous function">::
      (fun context ->
        let body = C.Node.NumLit 5.0 in
        let fn = C.Node.Fn ([], body) in
        let lua_stmt = Lua_stmt.make_expr "__fn1()" in
        let fun_str = "__fn1 = function()\nreturn 5.000000\nend" in
        let preamble = Lua_stmt.make_stmt "__fn1" fun_str in
        assert_equal
          (emit_node (C.Node.Apply (fn, [])))
          (Lua_stmt.insert_preamble lua_stmt preamble)
      );

    "emit cast expression">::
      (fun context ->
        assert_equal
          (emit_node (C.Node.NumLit 2.0))
          (Lua_stmt.make_expr "2.000000")
      );
  ]
