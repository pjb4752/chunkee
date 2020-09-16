open Frontend

let compiler_name =
  let root = Module_name.Segment.from_string "core" in
  let path = Module_name.Path.from_segments [root] in
  let name = Module_name.Segment.from_string "common" in
  Module_name.from_path_and_base path name

let constants = [
  Lua_definition.make_constant "true" Type.Bool;
  Lua_definition.make_constant "false" Type.Bool;
]

let functions = [
  Lua_definition.make_function "print" (Type.Function ([Type.String], Type.Unit))
]

let arithmatic_type = Type.Function ([Type.Number; Type.Number], Type.Number)
let logic_type = Type.Function ([Type.Number; Type.Number], Type.Bool)

let arithmatic_operators = [
  Lua_definition.make_operator "+" "core_common.add" arithmatic_type;
  Lua_definition.make_operator "-" "core_common.sub" arithmatic_type;
  Lua_definition.make_operator "*" "core_common.mlt" arithmatic_type;
  Lua_definition.make_operator "/" "core_common.div" arithmatic_type;
  Lua_definition.make_operator "%" "core_common.mod" arithmatic_type;
]

let logic_operators = [
  Lua_definition.make_mapped_operator "==" "core_common.eq" "=" logic_type;
  Lua_definition.make_mapped_operator "=~" "core_common.neq" "not=" logic_type;
  Lua_definition.make_operator ">" "core_common.gtn" logic_type;
  Lua_definition.make_operator ">=" "core_common.gte" logic_type;
  Lua_definition.make_operator "<" "core_common.ltn" logic_type;
  Lua_definition.make_operator "<=" "core_common.lte" logic_type;
]

let operators = arithmatic_operators @ logic_operators

let definitions = constants @ functions @ operators
