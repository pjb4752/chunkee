open Chunkee
open Chunkee.Parse
open Chunkee.Resolve.Resolve
open OUnit2

let assert_true value = assert_equal value true

let root = Mod_name.Name.from_string "__test__"
let path = Mod_name.Path.from_list [root]

let name0 = Var.Name.from_string "name0"
let name1 = Var.Name.from_string "name1"
let name2 = Var.Name.from_string "name2"

let type0 = Type.Str

let name = Mod_name.Name.from_string "core"
let m0 = Module.from_parts path name
let m1 = Module.define_var m0 name0 type0
let m2 = Module.define_var m1 name1 type0
let module_name = Module.name m2

let s_sym0 = Node.SymLit "name0"
let s_sym1 = Node.SymLit "name1"

let n_sym0 = Node.SymLit (Name.Var.Module (module_name, name0))
let n_sym1 = Node.SymLit (Name.Var.Module (module_name, name1))

let t0 = Test_data.stdlib
let t1 = Symbol_table.insert_module t0 m2

let suite =
  "Resolve suite">::: [
    "resolve number literal">::
      (fun context ->
        assert_equal
          (resolve_node t1 m2 (Node.NumLit 55.0))
          (Ok (Node.NumLit 55.0))
      );

    "resolve string literal">::
      (fun context ->
        assert_equal
          (resolve_node t1 m2 (Node.StrLit "hi"))
          (Ok (Node.StrLit "hi"))
      );

    "resolve symbol literal of var name">::
      (fun context ->
        assert_equal
          (resolve_node t1 m2 (Node.SymLit "name0"))
          (Ok (Node.SymLit (Name.Var.Module (module_name, name0))))
      );

    "resolve symbol literal of qualified name">::
      (fun context ->
        let parts = List.map Mod_name.Name.from_string ["test"] in
        let path = Mod_name.Path.from_list parts in
        let name = Mod_name.Name.from_string "pervasive" in
        let module_name = Mod_name.make path name in
        let var_name = Var.Name.from_string "true" in
        assert_equal
          (resolve_node t1 m2 (Node.SymLit "test.pervasive/true"))
          (Ok (Node.SymLit (Name.Var.Module (module_name, var_name))))
      );

    "resolve undefined symbol literal">::
      (fun context ->
        let resolved = resolve_node t1 m2 (Node.SymLit "hooie") in
        assert_true (Thwack.Result.is_error resolved)
      );

    "resolve symbol literal in def">::
      (fun context ->
        let name = Node.Name.from_string "name2" in
        assert_equal
          (resolve_node t1 m2 (Node.Def (name, s_sym0)))
          (Ok (Node.Def (name, n_sym0)))
      );

    "resolve local symbol literal in fn">::
      (fun context ->
        let name = Node.VarDef.Name.from_string "p1" in
        let type0 = Node.TypeDef.from_string "num" in
        let param0 = Node.VarDef.from_parts name type0 in
        let param1 = Node.VarDef.from_parts name Type.Num in
        assert_equal
          (resolve_node t1 m2 (Node.Fn ([param0], Node.SymLit "p1")))
          (Ok (Node.Fn ([param1], Node.SymLit (Name.Var.Local "p1"))))
      );

    "resolve local symbol that shadows module var in fn">::
      (fun context ->
        let name = Node.VarDef.Name.from_string "name0" in
        let type0 = Node.TypeDef.from_string "num" in
        let param0 = Node.VarDef.from_parts name type0 in
        let param1 = Node.VarDef.from_parts name Type.Num in
        assert_equal
          (resolve_node t1 m2 (Node.Fn ([param0], Node.SymLit "name0")))
          (Ok (Node.Fn ([param1], Node.SymLit (Name.Var.Local "name0"))))
      );

    "resolve module symbol in fn">::
      (fun context ->
        let params = [] in
        assert_equal
          (resolve_node t1 m2 (Node.Fn (params, Node.SymLit "name0")))
          (Ok (Node.Fn (params, n_sym0)))
      );

    "resolve symbol literal in if">::
      (fun context ->
        assert_equal
          (resolve_node t1 m2 (Node.If (s_sym0, s_sym1, s_sym1)))
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
          (resolve_node t1 m2 (Node.Let (s_bindings, Node.SymLit "p1")))
          (Ok (Node.Let (n_bindings, Node.SymLit (Name.Var.Local "p1"))))
      );

    "resolve symbol literals in apply">::
      (fun context ->
        assert_equal
          (resolve_node t1 m2 (Node.Apply (s_sym0, [s_sym1])))
          (Ok (Node.Apply (n_sym0, [n_sym1])))
      );

    "resolve symbol literals in cast">::
      (fun context ->
        let typedef = Node.TypeDef.from_string "num" in
        assert_equal
          (resolve_node t1 m2 (Node.Cast (typedef, s_sym1)))
          (Ok (Node.Cast (Type.Num, n_sym1)))
      );

  ]
