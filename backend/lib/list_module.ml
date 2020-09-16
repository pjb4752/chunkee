open Frontend

let compiler_name =
  let root = Module_name.Segment.from_string "core" in
  let path = Module_name.Path.from_segments [root] in
  let name = Module_name.Segment.from_string "list" in
  Module_name.from_path_and_base path name

let functions = [
  Lua_definition.make_function "empty" (Type.Function([], Type.List));
  Lua_definition.make_function "cons" (Type.Function ([Type.List; Type.Any], Type.List));
  Lua_definition.make_function "empty?" (Type.Function ([Type.List], Type.Bool));
  Lua_definition.make_function "head" (Type.Function ([Type.List], Type.Any));
  Lua_definition.make_function "tail" (Type.Function ([Type.List], Type.List));
  Lua_definition.make_function "nth" (Type.Function ([Type.List; Type.Number], Type.Any));
]

let definitions = functions
