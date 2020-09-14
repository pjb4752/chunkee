open Frontend

let name =
  let root = Module_name.Segment.from_string "core" in
  let path = Module_name.Path.from_segments [root] in
  let name = Module_name.Segment.from_string "common" in
  Module_name.from_path_and_base path name

let constants = [
  Lua_var.make_const "true" Type.Bool;
  Lua_var.make_const "false" Type.Bool;
]

let functions = [
  Lua_var.make_fn "print" (Type.Function ([Type.String], Type.Unit))
]

let math_fntype = Type.Function ([Type.Number; Type.Number], Type.Number)
let bool_fntype = Type.Function ([Type.Number; Type.Number], Type.Bool)

let math_operators = [
  Lua_var.make_simple_op "+" "core_common.add" math_fntype;
  Lua_var.make_simple_op "-" "core_common.sub" math_fntype;
  Lua_var.make_simple_op "*" "core_common.mlt" math_fntype;
  Lua_var.make_simple_op "/" "core_common.div" math_fntype;
  Lua_var.make_simple_op "%" "core_common.mod" math_fntype;
]

let bool_operators = [
  Lua_var.make_mapped_op "==" "core_common.eq" "=" bool_fntype;
  Lua_var.make_simple_op ">" "core_common.gtn" bool_fntype;
  Lua_var.make_simple_op ">=" "core_common.gte" bool_fntype;
  Lua_var.make_simple_op "<" "core_common.ltn" bool_fntype;
  Lua_var.make_simple_op "<=" "core_common.lte" bool_fntype;
  Lua_var.make_mapped_op "=~" "core_common.neq" "not=" bool_fntype;
]

let operators = math_operators @ bool_operators

let vars = constants @ functions @ operators
