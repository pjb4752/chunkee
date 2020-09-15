open Frontend
open Common.Extensions

type t = {
  name: Module_name.t;
  variables: Lua_var.t list;
}

let make name variables = { name; variables }

let name { name; _ } = name

let find_operator { variables; _ } compiler_name =
  let rec find' = function
    | [] -> None
    | current_variable :: remaining_variables -> begin
      match Lua_var.lua_operator current_variable compiler_name with
      | Some lua_operator -> Some lua_operator
      | None -> find' remaining_variables
    end in
  find' variables

let operator_exists modul compiler_name =
  Option.is_some @@ find_operator modul compiler_name

let compiler_variable compiler_module variable =
  let compiler_name = Lua_var.compiler_name variable in
  let compiler_type = Lua_var.compiler_type variable in
  let compiler_name = Identifier.from_string compiler_name in
  Module.define_var compiler_module compiler_name compiler_type

let to_compiler_module { name; variables } =
  let mempty = Module.with_name name in
  List.fold_left compiler_variable mempty variables
