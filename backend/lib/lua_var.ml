open Frontend

type definition_t =
  | Constant of string
  | Function of string
  | Operator of Lua_operator.t

type t = {
  definition: definition_t;
  compiler_type: Type.t;
}

let make_constant constant_value constant_type =
  { definition = Constant constant_value; compiler_type = constant_type }

let make_function function_name function_type =
  { definition = Function function_name; compiler_type = function_type }

let make_simple_operator name wrapper_name operator_type =
  let operator = Lua_operator.make_simple name wrapper_name in
  { definition = Operator operator; compiler_type = operator_type }

let make_mapped_operator lua_name wrapper_name compiler_name operator_type =
  let operator = Lua_operator.make_mapped lua_name wrapper_name compiler_name in
  { definition = Operator operator; compiler_type = operator_type }

let lua_name { definition; _ } =
  match definition with
  | Constant value -> value
  | Function name -> name
  | Operator operator -> Lua_operator.lua_name operator

let compiler_name { definition; _ } =
  match definition with
  | Constant value -> value
  | Function name -> name
  | Operator operator -> Lua_operator.compiler_name operator

let compiler_type { compiler_type; _ } = compiler_type

let lua_operator { definition; _ } compiler_name =
  match definition with
  | Operator operator when (Lua_operator.compiler_name operator) = compiler_name -> Some operator
  | Operator _ | Constant _ | Function _ -> None
