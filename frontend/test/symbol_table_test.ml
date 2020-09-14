open Chunkee
open Chunkee.Symbol_table
open OUnit2

let path0 = Mod_name.Name.from_string "path0"
let path1 = Mod_name.Name.from_string "path1"
let path2 = Mod_name.Name.from_string "path2"

let path = Mod_name.Path.from_list [path0; path1; path2]

let name0 = Mod_name.Name.from_string "name0"
let name1 = Mod_name.Name.from_string "name1"
let module0 = Module.from_parts path name0
let module1 = Module.from_parts path name1

let table = make Test_data.pervasive

let suite =
  "Symbol_table suite">::: [
    (*"inserting a nested module and re-finding it">::
      (fun context ->
        let table = insert_module table module0 in
        let name = Module.name module0 in
        let found = find_module table name in
        assert_equal (Some module0) found
      );

    "inserting a nested module with a similar path">::
      (fun context ->
        let t0 = insert_module table module0 in
        let t1 = insert_module t0 module1 in
        let name = Module.name module1 in
        let found = find_module t1 name in
        assert_equal (Some module1) found
      );

    "updating a nested module">::
      (fun context ->
        let name = Var.Name.from_string "hi" in
        let t0 = insert_module table module0 in
        let m1 = Module.define_var module0 name Type.Num in
        let table = update_module t0 m1 in
        let name = Module.name m1 in
        let found = find_module table name in
        assert_equal (Some m1) found
      );
      *)
  ]
