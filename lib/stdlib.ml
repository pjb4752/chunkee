let var name =
  Module.Var.Name.from_string name |> Module.Var.make

let make_module root name vars =
  let name = Module.Name.from_string name in
  let path = Module.Path.from_list [root] in
  let mempty = Module.make path name in
  List.fold_left Module.add_var mempty vars

let root = Module.Name.from_string "core"

let common_module =
  let name = "common"
  and vars = [
    var "+";
    var "-";
    var "*";
    var "/";
    var "print";
  ] in
  make_module root name vars

let modules =
  [
    common_module;
  ]

let global_path = Module.full_path common_module

let core = modules
