open Chunkee
open Chunkee.Table
open OUnit2

let path0 = Module.Name.from_string "path0"
let path1 = Module.Name.from_string "path1"
let path2 = Module.Name.from_string "path2"

let path = Module.Path.from_list [path0; path1; path2]

let name0 = Module.Name.from_string "name0"
let name1 = Module.Name.from_string "name1"
let module0 = Module.from_parts path name0
let module1 = Module.from_parts path name1

let suite =
  "Table suite">::: [
    "finding a nested module">::
      (fun context ->
        let empty = Children.empty in
        let t0 = Node (Children.add name0 (Leaf module0) empty) in
        let t1 = Node (Children.add path1 t0 empty) in
        let table = Node (Children.add path0 t1 empty) in
        let path = Module.Path.from_list [path0; path1] in
        let name = Module.Qual_name.make path name0 in
        let found = find_module table name in
        assert_equal (Some module0) found
      );

    "inserting a nested module">::
      (fun context ->
        let table = insert_module with_stdlib module0 in
        let name = Module.qual_name module0 in
        let found = find_module table name in
        assert_equal (Some module0) found
      );

    "inserting a nested module with a similar path">::
      (fun context ->
        let t0 = insert_module with_stdlib module0 in
        let t1 = insert_module t0 module1 in
        let name = Module.qual_name module1 in
        let found = find_module t1 name in
        assert_equal (Some module1) found
      );

    "updating a nested module">::
      (fun context ->
        let name = Module.Var.Name.from_string "hi" in
        let t0 = insert_module with_stdlib module0 in
        let m1 = Module.define_var module0 name Type.Num in
        let table = update_module t0 m1 in
        let name = Module.qual_name m1 in
        let found = find_module table name in
        assert_equal (Some m1) found
      );
  ]
