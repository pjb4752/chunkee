open OUnit2
open Tree

let path0 = Module.Name.from_string "path0"
let path1 = Module.Name.from_string "path1"
let path2 = Module.Name.from_string "path2"

let name0 = Module.Name.from_string "name0"
let name1 = Module.Name.from_string "name1"
let module0 = Module.make name0
let module1 = Module.make name1

let suite =
  "Tree suite">::: [
    "finding a module at top level">::
      (fun context ->
        let empty = Children.empty in
        let tree = Node (Children.add name0 (Leaf module0) empty) in
        let found = find_module tree [name0] in
        assert_equal (Some module0) found
      );

    "finding a nested module">::
      (fun context ->
        let empty = Children.empty in
        let t0 = Node (Children.add name0 (Leaf module0) empty) in
        let t1 = Node (Children.add path1 t0 empty) in
        let tree = Node (Children.add path0 t1 empty) in
        let found = find_module tree [path0; path1; name0] in
        assert_equal (Some module0) found
      );

    "inserting a module at top level">::
      (fun context ->
        let tree = insert_module (make ()) [name0] module0 in
        let found = find_module tree [name0] in
        assert_equal (Some module0) found
      );

    "inserting a nested module">::
      (fun context ->
        let tree = insert_module (make ()) [path1; path0; name0] module0 in
        let found = find_module tree [path1; path0; name0] in
        assert_equal (Some module0) found
      );

    "inserting a nested module with a similar path">::
      (fun context ->
        let t0 = insert_module (make ()) [path1; path0; name0] module0 in
        let t1 = insert_module t0 [path1; path2; name1] module1 in
        let found = find_module t1 [path1; path2; name1] in
        assert_equal (Some module1) found
      );
  ]
