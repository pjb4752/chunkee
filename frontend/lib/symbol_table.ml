open Printf
open Common.Extensions.Option
open Common.Extensions.Option.Syntax
open Names

type t = {
  intrinsics: Intrinsics.t;
  module_tree: Module_tree.t;
  current_module: Module.t;
}

type err_t =
  | ModuleError of string
  | NameError of string

type exists_in_scope = string -> bool

type exists_in_decls = Identifier.t -> Type.t option

let make (intrinsics: Intrinsics.t) current_module =
  let module_tree = Module_tree.empty in
  let module_tree = Module_tree.insert_module module_tree intrinsics.common_module in
  { intrinsics; module_tree; current_module }

let current_module { current_module; _ } = current_module

let undefined_name_error name =
  Error (NameError (sprintf "%s is undefined" name))

let undefined_module_error module_name =
  let module_name = Module_name.to_string module_name in
  Error (ModuleError (sprintf "unknown module %s" module_name))

let find_module { module_tree; current_module; _ } module_name =
  if (Module.name current_module) = module_name then Some current_module
  else Module_tree.find_module module_tree module_name

let resolve_name_in_module module_to_search name =
  let name = Identifier.from_string name in
  let module_name = Module.name module_to_search in
  if Module.variable_exists module_to_search name then Ok (Resolved_name.ModuleName (module_name, name))
  else undefined_name_error (Identifier.to_string name)

let resolve_qualified_name symbol_table module_name name =
  match find_module symbol_table module_name with
  | Some module_to_search -> resolve_name_in_module module_to_search name
  | None -> undefined_module_error module_name

let resolve_unqualified_name { intrinsics; current_module; _ } name =
  match resolve_name_in_module current_module name with
  | Ok name -> Ok name
  | Error _ -> resolve_name_in_module intrinsics.common_module name

let resolve_name table exists_in_scope = function
  | Unresolved_name.QualifiedName (module_name, name) -> resolve_qualified_name table module_name name
  | Unresolved_name.UnqualifiedName name -> begin
    if exists_in_scope name then Ok (Resolved_name.LocalName (Identifier.from_string name))
    else resolve_unqualified_name table name
  end

let resolve_type_in_module module_to_search target_type =
  let name = Identifier.from_string target_type in
  match Module.find_type module_to_search name with
  | Some found_type -> Ok found_type
  | None -> undefined_name_error target_type

let resolve_qualified_type symbol_table module_name target_type =
  match find_module symbol_table module_name with
  | Some module_to_search -> resolve_type_in_module module_to_search target_type
  | None -> undefined_module_error module_name

let resolve_unqualified_type module_to_search lookup_fn target_type =
  match Type.find_builtin target_type with
  | Some found_type -> Ok found_type
  | None -> begin
    let name = Identifier.from_string target_type in
    let maybe_type = let* lookup_fn = lookup_fn in lookup_fn name in
    match maybe_type with
    | Some found_type -> Ok found_type
    | None -> resolve_type_in_module module_to_search target_type
  end

let resolve_simple_type symbol_table lookup_fn = function
  | Unresolved_name.QualifiedName (module_name, type_name) ->
      resolve_qualified_type symbol_table module_name type_name
  | Unresolved_name.UnqualifiedName type_name ->
      resolve_unqualified_type symbol_table.current_module lookup_fn type_name

let rec resolve_type symbol_table ?lookup_fn:(lookup_fn=None) = function
  | Type_expr.SimpleType type_name -> resolve_simple_type symbol_table lookup_fn type_name
  | Type_expr.CompoundType type_names ->
      let fold_fn resolved_types current_type =
        Common.Extensions.Result.(
          Syntax.(
            let* resolved_types = resolved_types in
            let* current_type = resolve_type symbol_table ~lookup_fn:lookup_fn current_type in
            return (current_type :: resolved_types))
          ) in
      match List.fold_left fold_fn (Ok []) type_names with
      | Error e -> Error e
      | Ok (return_type :: parameter_types) -> Ok (Type.Function (List.rev parameter_types, return_type))
      | Ok _ -> assert false

let find_variable symbol_table module_name variable_name =
  let* found_module = (find_module symbol_table module_name) in
  Module.find_variable found_module variable_name

let find_variable_type symbol_table module_name variable_name =
  let* variable = find_variable symbol_table module_name variable_name in
  return (Var.tipe variable)

let find_type symbol_table module_name type_name =
  let* found_module = (find_module symbol_table module_name) in
  Module.find_type found_module type_name

let define_variable symbol_table variable_name variable_type =
  let updated_module = Module.define_variable symbol_table.current_module variable_name variable_type in
  { symbol_table with current_module = updated_module }

let inspect { module_tree; current_module; _ } =
  let module_tree = Module_tree.inspect module_tree in
  let current_module = Module.inspect current_module in
  sprintf "SymbolTable(%s, %s)" module_tree current_module

let err_string = function
  | ModuleError message -> message
  | NameError message -> message
