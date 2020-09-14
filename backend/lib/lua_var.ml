open Frontend

type definition_t =
  | Constant of string
  | Function of string
  | Operator of Lua_operator.t

type t = {
  definition: definition_t;
  tipe: Type.t;
}

let make_const str tipe =
  { definition = Constant str; tipe }

let make_fn str tipe =
  { definition = Function str; tipe }

let make_simple_op name wrapper tipe =
  let operator = Lua_operator.make_simple name wrapper in
  { definition = Operator operator; tipe }

let make_mapped_op name wrapper mapping tipe =
  let operator = Lua_operator.make_mapped name wrapper mapping in
  { definition = Operator operator; tipe }

let name { definition; _ } =
  match definition with
  | Constant str -> str
  | Function str -> str
  | Operator op -> Lua_operator.name op

let compiler_name { definition; _ } =
  match definition with
  | Constant str -> str
  | Function str -> str
  | Operator op -> Lua_operator.compiler_name op

let tipe { tipe; _ } = tipe

let compiler_operator { definition; _ } compiler_name =
  let get_name = Lua_operator.compiler_name in
  match definition with
  | Operator lua_op when (get_name lua_op) = compiler_name -> Some lua_op
  | Operator _ | Constant _ | Function _ -> None
