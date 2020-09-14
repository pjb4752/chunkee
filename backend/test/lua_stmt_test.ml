open Kludge.Lua_stmt
open OUnit2

let suite =
  "Lua_stmt suite">::: [
    "simple expr to string">::
      (fun context ->
        let lua_stmt = make_expr "1 + 2" in
        assert_equal
          (to_result_string lua_stmt)
          "return 1 + 2"
      );

    "simple stmt to string">::
      (fun context ->
        let lua_stmt = make_stmt "__var1" "__var1 = 3" in
        assert_equal
          (to_result_string lua_stmt)
          "__var1 = 3\nreturn __var1"
      );

    "single preambled expr to string">::
      (fun context ->
        let lua_stmt = make_expr "fn(__var1, 2)"  in
        let preamble = make_stmt "__var1" "__var1 = 3" in
        let lua_stmt = insert_preamble lua_stmt preamble in
        assert_equal
          (to_result_string lua_stmt)
          "__var1 = 3\nreturn fn(__var1, 2)"
      );

    "single preambled stmt to string">::
      (fun context ->
        let lua_stmt = make_stmt "__var1" "__var1 = __var2"  in
        let preamble = make_stmt "__var2" "__var2 = 3" in
        let lua_stmt = insert_preamble lua_stmt preamble in
        assert_equal
          (to_result_string lua_stmt)
          "__var2 = 3\n__var1 = __var2\nreturn __var1"
      );

    "multiple preambled expr to string">::
      (fun context ->
        let lua_stmt = make_expr "fn(__var1, 2 + 3, __var2)"  in
        let preamble = make_stmt "__var1" "__var1 = 3" in
        let lua_stmt = insert_preamble lua_stmt preamble in
        let preamble = make_expr "2 + 3" in
        let lua_stmt = insert_preamble lua_stmt preamble in
        let preamble = make_stmt "__var2" "__var2 = 4" in
        let lua_stmt = insert_preamble lua_stmt preamble in
        assert_equal
          (to_result_string lua_stmt)
          "__var1 = 3\n__var2 = 4\nreturn fn(__var1, 2 + 3, __var2)"
      );

    "nested preambled expr to string">::
      (fun context ->
        let lua_stmt = make_expr "fn(__var1, 2 + __var2)"  in
        let preamble = make_stmt "__var1" "__var1 = 3" in
        let lua_stmt = insert_preamble lua_stmt preamble in
        let preamble = make_expr "2 + __var2" in
        let nested = make_stmt "__var2" "__var2 = 4" in
        let preamble = insert_preamble preamble nested in
        let lua_stmt = insert_preamble lua_stmt preamble in
        assert_equal
          (to_result_string lua_stmt)
          "__var1 = 3\n__var2 = 4\nreturn fn(__var1, 2 + __var2)"
      );

    "nested preambled stmt to string">::
      (fun context ->
        let lua_stmt = make_stmt "__var1" "__var1 = 2 + __var2"  in
        let preamble = make_expr "2 + __var2" in
        let nested = make_stmt "__var2" "__var2 = 4" in
        let preamble = insert_preamble preamble nested in
        let lua_stmt = insert_preamble lua_stmt preamble in
        assert_equal
          (to_result_string lua_stmt)
          "__var2 = 4\n__var1 = 2 + __var2\nreturn __var1"
      );
  ]
