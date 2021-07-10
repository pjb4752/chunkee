open Frontend
open Common.Extensions

type t = {
  compiler_name: Module_name.t;
  definitions: Lua_definition.t list;
}

let make compiler_name definitions = { compiler_name; definitions }

let find_operator { definitions; _ } compiler_name =
  let rec find' = function
    | [] -> None
    | current_definition :: remaining_definitions -> begin
      match Lua_definition.find_operator current_definition compiler_name with
      | Some lua_operator -> Some lua_operator
      | None -> find' remaining_definitions
    end in
  find' definitions

let operator_exists lua_module compiler_name =
  Option.is_some @@ find_operator lua_module compiler_name

let compiler_name { compiler_name; _ } = compiler_name

let to_compiler_module { compiler_name; definitions } =
  let make_compiler_variable compiler_module definition =
    let compiler_name = Lua_definition.compiler_name definition in
    let compiler_type = Lua_definition.compiler_type definition in
    let compiler_name = Identifier.from_string compiler_name in
    Module.define_variable compiler_module compiler_name compiler_type
  in
  let mempty = Module.with_name compiler_name in
  List.fold_left make_compiler_variable mempty definitions
