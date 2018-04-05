open Parse
open Resolve
open OUnit2

let assert_true value = assert_equal value true

let module_name = Module.Name.from_string "__test__"

let name0 = Module.Var.Name.from_string "name0"
let name1 = Module.Var.Name.from_string "name1"
let name2 = Module.Var.Name.from_string "name2"

let var0 = Module.Var.make name0
let var1 = Module.Var.make name1

let m0 = Module.make module_name
let m1 = Module.add_var m0 var0
let m2 = Module.add_var m1 var1

let s_sym0 = Node.SymLit "name0"
let s_sym1 = Node.SymLit "name1"

let n_sym0 = Node.SymLit (Name.Module name0)
let n_sym1 = Node.SymLit (Name.Module name1)

let suite =
  "Resolve suite">::: [
    "resolve number literal">::
      (fun context ->
        assert_equal
          (resolve_node m2 (Node.NumLit 55.0))
          (Ok (Node.NumLit 55.0))
      );

    "resolve string literal">::
      (fun context ->
        assert_equal
          (resolve_node m2 (Node.StrLit "hi"))
          (Ok (Node.StrLit "hi"))
      );

    "resolve symbol literal of var name">::
      (fun context ->
        assert_equal
          (resolve_node m2 (Node.SymLit "name0"))
          (Ok (Node.SymLit (Name.Module name0)))
      );

    "resolve undefined symbol literal">::
      (fun context ->
        let resolved = resolve_node m2 (Node.SymLit "hooie") in
        assert_true (Result.is_error resolved)
      );

    "resolve symbol literal in def">::
      (fun context ->
        assert_equal
          (resolve_node m2 (Node.Def (name2, s_sym0)))
          (Ok (Node.Def (name2, n_sym0)))
      );

    "resolve local symbol literal in fn">::
      (fun context ->
        let param0 = Node.Param.from_string "p1" in
        let params = [param0] in
        assert_equal
          (resolve_node m2 (Node.Fn (params, Node.SymLit "p1")))
          (Ok (Node.Fn (params, Node.SymLit (Name.Local "p1"))))
      );

    "resolve local symbol that shadows module var in fn">::
      (fun context ->
        let param0 = Node.Param.from_string "name0" in
        let params = [param0] in
        assert_equal
          (resolve_node m2 (Node.Fn (params, Node.SymLit "name0")))
          (Ok (Node.Fn (params, Node.SymLit (Name.Local "name0"))))
      );

    "resolve module symbol in fn">::
      (fun context ->
        let params = [] in
        assert_equal
          (resolve_node m2 (Node.Fn (params, Node.SymLit "name0")))
          (Ok (Node.Fn (params, n_sym0)))
      );

    "resolve symbol literal in if">::
      (fun context ->
        assert_equal
          (resolve_node m2 (Node.If (s_sym0, s_sym1, s_sym1)))
          (Ok (Node.If (n_sym0, n_sym1, n_sym1)))
      );

    "resolve symbol literals in let">::
      (fun context ->
        let sb_name0 = Node.Binding.Name.from_string "p1"
        and sb_expr0 = s_sym0 in
        let s_binding0 = Node.Binding.from_node sb_name0 sb_expr0 in
        let s_bindings = [s_binding0] in
        let nb_name0 = Node.Binding.Name.from_string "p1"
        and nb_expr0 = n_sym0 in
        let n_binding0 = Node.Binding.from_node nb_name0 nb_expr0 in
        let n_bindings = [n_binding0] in
        assert_equal
          (resolve_node m2 (Node.Let (s_bindings, Node.SymLit "p1")))
          (Ok (Node.Let (n_bindings, Node.SymLit (Name.Local "p1"))))
      );

    "resolve symbol literals in apply">::
      (fun context ->
        assert_equal
          (resolve_node m2 (Node.Apply (s_sym0, [s_sym1])))
          (Ok (Node.Apply (n_sym0, [n_sym1])))
      );
  ]
