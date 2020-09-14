open Frontend

let name =
  let root = Module_name.Segment.from_string "core" in
  let path = Module_name.Path.from_segments [root] in
  let name = Module_name.Segment.from_string "list" in
  Module_name.from_path_and_base path name

let functions = [
  Lua_var.make_fn "empty" (Type.Function([], Type.List));
  Lua_var.make_fn "cons" (Type.Function ([Type.List; Type.Any], Type.List));
  Lua_var.make_fn "empty?" (Type.Function ([Type.List], Type.Bool));
  Lua_var.make_fn "head" (Type.Function ([Type.List], Type.Any));
  Lua_var.make_fn "tail" (Type.Function ([Type.List], Type.List));
  Lua_var.make_fn "nth" (Type.Function ([Type.List; Type.Number], Type.Any));
]

let vars = functions
