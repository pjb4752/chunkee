type state = Lua_api_lib.state

type error =
  | Syntax_err
  | Memory_err
  | Runtime_err
  | Generic_err

type status = (unit, error) result

val new_state: unit -> state

val eval: state -> string -> status
