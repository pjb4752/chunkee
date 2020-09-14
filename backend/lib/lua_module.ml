open Frontend
open Common.Extensions

type compiler_module_t = Module.t

type t = {
  name: Module_name.t;
  vars: Lua_var.t list;
}

let make name vars = { name; vars }

let name { name; _ } = name

let find_operator { vars; _ } compiler_name =
  let rec find' = function
    | [] -> None
    | var :: vars -> begin
      match Lua_var.compiler_operator var compiler_name with
      | Some lua_operator -> Some lua_operator
      | None -> find' vars
    end in
  find' vars

let operator_exists modul compiler_name =
  Option.is_some @@ find_operator modul compiler_name

let compiler_var modul var =
  let name = Lua_var.compiler_name var and tipe = Lua_var.tipe var in
  let name = Identifier.from_string name in
  Module.define_var modul name tipe

let to_compiler_module { name; vars } =
  let mempty = Module.with_name name in
  List.fold_left compiler_var mempty vars
