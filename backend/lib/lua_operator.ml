type t = {
  lua_name: string;
  compiler_name: string;
  wrapper_name: string;
}

let make_simple name wrapper_name =
  { lua_name = name; compiler_name = name; wrapper_name; }

let make_mapped lua_name wrapper_name compiler_name =
  { lua_name; compiler_name; wrapper_name }

let lua_name { lua_name; _ } = lua_name

let compiler_name { compiler_name; _ } = compiler_name

let wrapper_name { wrapper_name; _ } = wrapper_name
