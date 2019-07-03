open Chunkee
open Chunkee.Typecheck
open OUnit2

let root = Mod_name.Name.from_string "__test__"
let path = Mod_name.Path.from_list [root]

let name0 = Var.Name.from_string "name0"
let name1 = Var.Name.from_string "name1"
let name2 = Var.Name.from_string "true?"
let name3 = Var.Name.from_string "value"
let name4 = Var.Name.from_string "fnguy"

let name = Mod_name.Name.from_string "core"
let m0 = Module.from_parts path name
let m1 = Module.define_var m0 name0 Type.Str
let m2 = Module.define_var m1 name1 Type.Num
let m3 = Module.define_var m2 name2 Type.Bool
let m4 = Module.define_var m3 name3 Type.Any
let mfinal = Module.define_var m4 name4 (Type.Fn ([Type.Any], Type.Num))
let module_name = Module.name m3

let n_sym0 = Node.SymLit (Name.Var.Module (module_name, name0))
let n_sym1 = Node.SymLit (Name.Var.Module (module_name, name1))
let n_sym2 = Node.SymLit (Name.Var.Module (module_name, name2))
let n_sym3 = Node.SymLit (Name.Var.Module (module_name, name3))
let n_sym4 = Node.SymLit (Name.Var.Module (module_name, name4))

let t0 = Symbol_table.make Test_data.pervasive
(*let t1 = Symbol_table.insert_module t0 mfinal*)

let suite =
  "Typecheck suite">::: [
    (*
    "finding type of number literal">::
      (fun context ->
        assert_equal
          (check_node t1 mfinal (Node.NumLit 55.0))
          (Ok (mfinal, Type.Num))
      );

    "finding type of string literal">::
      (fun context ->
        assert_equal
          (check_node t1 mfinal (Node.StrLit "hello"))
          (Ok (mfinal, Type.Str))
      );

    "finding type of module name">::
      (fun context ->
        assert_equal
        (check_node t1 mfinal n_sym0)
        (Ok (mfinal, Type.Str))
      );

    "finding type of def form">::
      (fun context ->
        let name = Node.Name.from_string "zzz" in
        let var_name = Var.Name.from_string "zzz" in
        let mafter = Module.define_var mfinal var_name Type.Num in
        assert_equal
          (check_node t1 mfinal (Node.Def (name, Node.NumLit 55.0)))
          (Ok (mafter, Type.Num))
      );

    "finding type of fn form">::
      (fun context ->
        let name0 = Node.VarDef.Name.from_string "p1" in
        let param0 = Node.VarDef.from_parts name0 Type.Num in
        assert_equal
          (check_node t1 mfinal (Node.Fn ([param0], Node.StrLit "hi")))
          (Ok (mfinal, (Type.Fn ([Type.Num], Type.Str))))
      );

    "finding type of if form">::
      (fun context ->
        let iff = Node.StrLit "hi"
        and els = Node.StrLit "bye" in
        assert_equal
          (check_node t1 mfinal (Node.If (n_sym2, iff, els)))
          (Ok (mfinal, Type.Str))
      );

    "finding type of if form with an any result">::
      (fun context ->
        let iff = Node.StrLit "hi"
        and els = n_sym3 in
        assert_equal
          (check_node t1 mfinal (Node.If (n_sym2, iff, els)))
          (Ok (mfinal, Type.Any))
      );

    "finding type of let form">::
      (fun context ->
        let name0 = Node.Binding.Name.from_string "p1"
        and expr0 = Node.NumLit 55.0 in
        let name1 = Node.Binding.Name.from_string "p2"
        and expr1 = Node.NumLit 10.0 in
        let binding0 = Node.Binding.from_node name0 expr0 in
        let binding1 = Node.Binding.from_node name1 expr1 in
        let bindings = [binding0; binding1] in
        let fn_name = Var.Name.from_string "+" in
        let module_name = Test_data.pervasive_name in
        let name = Name.Var.Module (module_name, fn_name) in
        let expr =
          Node.Apply (Node.SymLit name, [
            Node.SymLit (Name.Var.Local "p1");
            Node.SymLit (Name.Var.Local "p2")
          ]) in
        assert_equal
          (check_node t1 mfinal (Node.Let (bindings, expr)))
          (Ok (mfinal, Type.Num))
      );

    "finding type of apply form">::
      (fun context ->
        let fn_name = Var.Name.from_string "+" in
        let module_name = Test_data.pervasive_name in
        let name = Name.Var.Module (module_name, fn_name) in
        let args = [Node.NumLit 55.0; Node.NumLit 10.0] in
        assert_equal
          (check_node t1 mfinal (Node.Apply (Node.SymLit name, args)))
          (Ok (mfinal, Type.Num))
      );

    "finding type of apply form with a function taking any params">::
      (fun context ->
        let args = [Node.NumLit 55.0] in
        assert_equal
          (check_node t1 mfinal (Node.Apply (n_sym4, args)))
          (Ok (mfinal, Type.Num))
      );

    "finding type of cast form">::
      (fun context ->
        assert_equal
          (check_node t1 mfinal (Node.Cast (Type.Num, n_sym0)))
          (Ok (mfinal, Type.Num))
      );
      *)
  ]
