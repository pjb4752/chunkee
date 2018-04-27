let make_var modul (name, t) =
  let name = Module.Var.Name.from_string name in
  Module.define_var modul name t

let make_module root name vars =
  let name = Module.Name.from_string name in
  let path = Module.Path.from_list [root] in
  let mempty = Module.from_parts path name in
  List.fold_left make_var mempty vars

let root = Module.Name.from_string "core"

let math_fntype = Type.Fn ([Type.Num; Type.Num], Type.Num)

let common_module =
  let name = "common"
  and vars = [
    ("+", math_fntype);
    ("-", math_fntype);
    ("*", math_fntype);
    ("/", math_fntype);
    ("print", Type.Fn([Type.Str], Type.Unit));
  ] in
  make_module root name vars

let modules =
  [
    common_module;
  ]

let global_name = Module.qual_name common_module

let core = modules
