open OUnit2
open Typecheck

let root = Module.Name.from_string "__test__"
let path = Module.Path.from_list [root]

let name0 = Module.Var.Name.from_string "name0"
let name1 = Module.Var.Name.from_string "name1"
let name2 = Module.Var.Name.from_string "true?"

let name = Module.Name.from_string "core"
let m0 = Module.from_parts path name
let m1 = Module.define_var m0 name0 Type.Str
let m2 = Module.define_var m1 name1 Type.Num
let m3 = Module.define_var m2 name2 Type.Bool
let qual_name = Module.qual_name m3

let n_sym0 = Node.SymLit (Name.Module (qual_name, name0))
let n_sym1 = Node.SymLit (Name.Module (qual_name, name1))
let n_sym2 = Node.SymLit (Name.Module (qual_name, name2))

let t0 = Table.with_stdlib
let t1 = Table.insert_module t0 m3

let suite =
  "Typecheck suite">::: [
    "finding type of number literal">::
      (fun context ->
        assert_equal
          (check_node t1 m3 (Node.NumLit 55.0))
          (Ok Type.Num)
      );

    "finding type of string literal">::
      (fun context ->
        assert_equal
          (check_node t1 m3 (Node.StrLit "hello"))
          (Ok Type.Str)
      );

    "finding type of module name">::
      (fun context ->
        assert_equal
        (check_node t1 m3 n_sym0)
        (Ok Type.Str)
      );

    "finding type of def form">::
      (fun context ->
        let name = Node.VarDef.Name.from_string "name1"
        and t = Node.VarDef.Type.from_string "num" in
        let var = Node.VarDef.from_parts name t in
        assert_equal
          (check_node t1 m3 (Node.Def (var, Node.NumLit 55.0)))
          (Ok Type.Num)
      );

    "finding type of fn form">::
      (fun context ->
        let name0 = Node.VarDef.Name.from_string "p1"
        and type0 = Node.VarDef.Type.from_string "num" in
        let param0 = Node.VarDef.from_parts name0 type0 in
        let params = [param0] in
        assert_equal
          (check_node t1 m3 (Node.Fn (params, Node.StrLit "hi")))
          (Ok (Type.Fn ([Type.Num], Type.Str)))
      );

    "finding type of if form">::
      (fun context ->
        let iff = Node.StrLit "hi"
        and els = Node.StrLit "bye" in
        assert_equal
          (check_node t1 m3 (Node.If (n_sym2, iff, els)))
          (Ok Type.Str)
      );

    "finding type of let form">::
      (fun context ->
        let name0 = Node.Binding.Name.from_string "p1"
        and expr0 = Node.NumLit 55.0 in
        let name1 = Node.Binding.Name.from_string "p2"
        and expr1 = Node.NumLit 10.0 in
        let binding0 = Node.Binding.from_node name0 expr0
        and binding1 = Node.Binding.from_node name1 expr1 in
        let bindings = [binding0; binding1] in
        let fn_name = Module.Var.Name.from_string "+"
        and qual_name = Stdlib.global_name in
        let name = Name.Module (qual_name, fn_name) in
        let expr =
          Node.Apply (Node.SymLit name, [
            Node.SymLit (Name.Local "p1");
            Node.SymLit (Name.Local "p2")
          ]) in
        assert_equal
          (check_node t1 m3 (Node.Let (bindings, expr)))
          (Ok Type.Num)
      );

    "finding type of apply form">::
      (fun context ->
        let fn_name = Module.Var.Name.from_string "+"
        and qual_name = Stdlib.global_name in
        let name = Name.Module (qual_name, fn_name) in
        let args = [Node.NumLit 55.0; Node.NumLit 10.0] in
        assert_equal
          (check_node t1 m3 (Node.Apply (Node.SymLit name, args)))
          (Ok Type.Num)
      );

  ]
