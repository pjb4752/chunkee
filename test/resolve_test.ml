open Chunkee
open Chunkee.Resolve
open OUnit2
open Test_helper

let assert_true value = assert_equal value true

let root = Mod_name.Name.from_string "__test__"
let path = Mod_name.Path.from_list [root]

let name0 = Var.Name.from_string "name0"
let name1 = Var.Name.from_string "name1"
let name2 = Var.Name.from_string "name2"

let type0 = Type.Str

let base_name = Mod_name.Name.from_string "core"
let m0 = Module.from_parts path base_name
let m1 = Module.define_var m0 name0 type0
let m2 = Module.define_var m1 name1 type0
let module_name = Module.name m2

let s_sym0 = make_bare_sym "name0"
let s_sym1 = make_bare_sym "name1"

let n_sym0 = make_mod_sym module_name name0
let n_sym1 = make_mod_sym module_name name1

let t0 = Test_data.stdlib
let t1 = Symbol_table.insert_module t0 m2

let suite =
  "Resolve suite">::: [
    "resolve number literal">::
      (fun context ->
        assert_equal
          (resolve_node t1 m2 (PNode.NumLit 55.0))
          (Ok (RNode.NumLit 55.0))
      );

    "resolve string literal">::
      (fun context ->
        assert_equal
          (resolve_node t1 m2 (PNode.StrLit "hi"))
          (Ok (RNode.StrLit "hi"))
      );

    "resolve symbol literal of var name">::
      (fun context ->
        assert_equal
          (resolve_node t1 m2 (make_bare_sym "name0"))
          (Ok (make_mod_sym module_name name0))
      );

    "resolve symbol literal of qualified name">::
      (fun context ->
        let parts = List.map Mod_name.Name.from_string ["test"] in
        let path = Mod_name.Path.from_list parts in
        let name = Mod_name.Name.from_string "pervasive" in
        let module_name = Mod_name.make path name in
        let var_name = Var.Name.from_string "true" in
        assert_equal
          (resolve_node t1 m2 (make_qual_sym "test.pervasive" "true"))
          (Ok (make_mod_sym module_name var_name))
      );

    "resolve undefined symbol literal">::
      (fun context ->
        let resolved = resolve_node t1 m2 (make_bare_sym "hooie") in
        assert_true (Thwack.Result.is_error resolved)
      );

    "resolve symbol literal in def">::
      (fun context ->
        let name = PNode.Name.from_string "name2" in
        assert_equal
          (resolve_node t1 m2 (PNode.Def (name, s_sym0)))
          (Ok (RNode.Def (name, n_sym0)))
      );

    "resolve local symbol literal in fn">::
      (fun context ->
        let param0 = make_p_var_def "p1" "num" in
        let param1 = make_r_var_def "p1" Type.Num in
        assert_equal
          (resolve_node t1 m2 (PNode.Fn ([param0], make_bare_sym "p1")))
          (Ok (RNode.Fn ([param1], make_local_sym "p1")))
      );

    "resolve local symbol that shadows module var in fn">::
      (fun context ->
        let param0 = make_p_var_def "name0" "num" in
        let param1 = make_r_var_def "name0" Type.Num in
        assert_equal
          (resolve_node t1 m2 (PNode.Fn ([param0], make_bare_sym "name0")))
          (Ok (RNode.Fn ([param1], make_local_sym "name0")))
      );

    "resolve module symbol in fn">::
      (fun context ->
        let params = [] in
        assert_equal
          (resolve_node t1 m2 (PNode.Fn (params, make_bare_sym "name0")))
          (Ok (RNode.Fn (params, n_sym0)))
      );

    "resolve symbol literal in if">::
      (fun context ->
        assert_equal
          (resolve_node t1 m2 (PNode.If (s_sym0, s_sym1, s_sym1)))
          (Ok (RNode.If (n_sym0, n_sym1, n_sym1)))
      );

    "resolve symbol literals in let">::
      (fun context ->
        let s_bindings = [make_p_binding "p1" s_sym0] in
        let n_bindings = [make_r_binding "p1" n_sym0] in
        assert_equal
          (resolve_node t1 m2 (PNode.Let (s_bindings, make_bare_sym "p1")))
          (Ok (RNode.Let (n_bindings, make_local_sym "p1")))
      );

    "resolve symbol literals in apply">::
      (fun context ->
        assert_equal
          (resolve_node t1 m2 (PNode.Apply (s_sym0, [s_sym1])))
          (Ok (RNode.Apply (n_sym0, [n_sym1])))
      );

    "resolve symbol literals in cast">::
      (fun context ->
        let type_expr = make_type_expr "num" in
        assert_equal
          (resolve_node t1 m2 (PNode.Cast (type_expr, s_sym1)))
          (Ok (RNode.Cast (Type.Num, n_sym1)))
      );
  ]
