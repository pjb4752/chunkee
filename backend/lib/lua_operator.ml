type t = {
  name: string;
  compiler_name: string;
  wrapper_name: string;
}

let make_simple name wrapper_name =
  { name; compiler_name = name; wrapper_name; }

let make_mapped name wrapper_name compiler_name =
  { name; compiler_name; wrapper_name }

let name { name; _ } = name

let compiler_name { compiler_name; _ } = compiler_name

let wrapper { wrapper_name; _ } = wrapper_name
