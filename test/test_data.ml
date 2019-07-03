module C = Chunkee

let pervasive_root = C.Mod_name.Name.from_string "test"

let vars = [
  ("true", C.Type.Bool);
  ("false", C.Type.Bool);
  ("+", C.Type.Fn ([C.Type.Num; C.Type.Num], C.Type.Num))
]

let pervasive_module =
  let name = C.Mod_name.Name.from_string "pervasive" in
  let path = C.Mod_name.Path.from_list [pervasive_root] in
  let mempty = C.Module.from_parts path name in
  List.fold_left (fun m (n, t) ->
    let n = C.Var.Name.from_string n in
    C.Module.define_var m n t) mempty vars

let pervasive_name = C.Module.name pervasive_module

let pervasive = {
  C.Pervasive.modul = pervasive_module;
}

let stdlib = C.Symbol_table.make pervasive
