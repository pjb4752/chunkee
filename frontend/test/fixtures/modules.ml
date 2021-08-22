module Module = Frontend.Module
module Module_name = Frontend.Module_name
module Type = Frontend.Type

module Common = struct
  let pi_name = "pi"
  let plus_name = "+"

  let name =
    let module_segments = List.map Module_name.Segment.from_string ["test"] in
    let module_path = Module_name.Path.from_segments module_segments in
    let module_base = Module_name.Segment.from_string "common" in
    Module_name.from_path_and_base module_path module_base

  let defined_module =
    let named_module = Module.with_name name in
    let named_module = Module.define_variable named_module "pi" Type.Number in
    Module.define_variable named_module "+" (Type.Function ([Type.Number; Type.Number], Type.Number))
end
