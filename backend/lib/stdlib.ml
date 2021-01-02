open Frontend

let common_module = Common_module.(Lua_module.make compiler_name definitions)

let list_module = List_module.(Lua_module.make compiler_name definitions)

let lua_modules = [
  common_module;
  list_module;
]

let find_lua_module name =
  List.find_opt (fun lua_module -> (Lua_module.compiler_name lua_module) = name) lua_modules

let intrinsics = { Intrinsics.common_module = Lua_module.to_compiler_module common_module }

let compiler_modules =
  List.fold_left (fun tree current_module ->
    Lua_module.to_compiler_module current_module |> Module_tree.insert_module tree
  ) Module_tree.empty lua_modules

(*let is_infix_op qual_name var_name =*)
  (*let name = C.Var.Name.to_string var_name in*)
  (*has_pervasive_name qual_name && List.mem name*)

(*let is_wrapped_op op =*)
  (*List.exists ((=) op) infix_operators*)

(*let wrapped_op op =*)
  (*let cmp_fn (o1, _) = o1 = op in*)
  (*let maybe_op = List.find_opt cmp_fn wrapped_operators in*)
  (*maybe_op >>= fun m -> return (snd m)*)
