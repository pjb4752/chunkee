type t = {
  name: string;
  compiler_name: string;
  wrapper_function_name: string;
}

let make name wrapper_function_name =
  { name; compiler_name = name; wrapper_function_name; }

let make_with_mapping name wrapper_function_name compiler_name =
  { name; compiler_name; wrapper_function_name }

let name { name; _ } = name

let compiler_name { compiler_name; _ } = compiler_name

let wrapper_function_name { wrapper_function_name; _ } = wrapper_function_name
