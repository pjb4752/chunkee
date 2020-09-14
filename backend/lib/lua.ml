open Common.Result

type state = Lua_api_lib.state

type error =
  | Syntax_err
  | Memory_err
  | Runtime_err
  | Generic_err

type status = (unit, error) result

let print_last_error lua_state =
  let () = Lua_api_lib.getglobal lua_state "print" in
  let () = Lua_api_lib.insert lua_state 1 in
  Lua_api_lib.call lua_state 1 0

let check_status lua_state thread_status =
  match thread_status with
  | Lua_api_lib.LUA_OK -> Ok ()
  | error -> begin
    let () = print_last_error lua_state in
    let error =
      match error with
      | Lua_api_lib.LUA_OK -> assert false
      | Lua_api_lib.LUA_ERRRUN -> Runtime_err
      | Lua_api_lib.LUA_ERRSYNTAX -> Syntax_err
      | Lua_api_lib.LUA_ERRMEM -> Memory_err
      | _ -> Generic_err in
    Error error
  end

let pcall lua_state nargs nret =
  let status = Lua_api_lib.pcall lua_state nargs nret 0 in
  check_status lua_state status

let load_string lua_state lua_str =
  let status = Lua_aux_lib.loadstring lua_state lua_str in
  check_status lua_state status

let print_stack_items lua_state index nargs =
  let () = Lua_api_lib.getglobal lua_state "print" in
  let () = Lua_api_lib.insert lua_state index in
  pcall lua_state nargs 0

let print_stack lua_state =
  let stacksize = Lua_api_lib.gettop lua_state in
  if stacksize = 0 then Ok ()
  else print_stack_items lua_state 1 stacksize

let exec_last lua_state =
  let result = pcall lua_state 0 Lua_api_lib.multret in
  result >>= fun _ -> (print_stack lua_state) >>= fun m -> return m

let new_state () =
  let lua_state = Lua_aux_lib.newstate () in
  let () = Lua_aux_lib.openlibs lua_state in
  lua_state

let eval lua_state lua_str =
  (load_string lua_state lua_str) >>= fun _ ->
  (exec_last lua_state) >>= fun ok ->
  return ok
