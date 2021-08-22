open Frontend.Ast
open Frontend.Identifier_resolution
open Frontend.Names
open OUnit2

module Intrinsics = Frontend.Intrinsics
module Module = Frontend.Module
module Module_name = Frontend.Module_name
module Symbol_table = Frontend.Symbol_table
module Type = Frontend.Type

let current_module_name =
  let module_segments = List.map Module_name.Segment.from_string ["test"] in
  let module_path = Module_name.Path.from_segments module_segments in
  let module_base = Module_name.Segment.from_string "current" in
  Module_name.from_path_and_base module_path module_base

let current_module =
  Module.with_name current_module_name

let common_module_name = Modules.Common.name

let common_module = Modules.Common.defined_module

let symbol_table = Symbol_table.make { Intrinsics.common_module } current_module

let pi_name = Modules.Common.pi_name

let assert_resolves_to ?symbol_table:(symbol_table=symbol_table) expected semantic_form =
  assert_equal ~printer:Result.inspect (Ok expected) (resolve_identifiers symbol_table semantic_form)

let suite =
  "Resolving suite">::: [
    "resolve number literal">::
      (fun _ ->
        assert_resolves_to Numbers.resolved_form Numbers.semantic_form
      );

    "resolve string literal">::
      (fun _ ->
        assert_resolves_to Strings.resolved_form Strings.semantic_form
      );

    "resolve symbol from common module">::
      (fun _ ->
        let position = Symbols.semantic_form.position in
        let pi_name = Resolved_name.ModuleName (common_module_name, pi_name) in
        let resolved_form = Resolved_form.create_symbol position pi_name in
        assert_resolves_to ~symbol_table:symbol_table resolved_form Symbols.semantic_form
      );

    "resolve symbol from current module that shadows common module">::
      (fun _ ->
        let symbol_table = Symbol_table.define_variable symbol_table pi_name Type.Bool in
        let position = Symbols.semantic_form.position in
        let pi_name = Resolved_name.ModuleName (current_module_name, pi_name) in
        let resolved_form = Resolved_form.create_symbol position pi_name in
        assert_resolves_to ~symbol_table:symbol_table resolved_form Symbols.semantic_form
      );

    "resolve symbol from qualified current module">::
      (fun _ ->
        let symbol_table = Symbol_table.define_variable symbol_table pi_name Type.Bool in
        let position = Symbols.semantic_form.position in
        let qualified_name = Unresolved_name.QualifiedName (current_module_name, "pi") in
        let semantic_form = Semantic_form.create_symbol position qualified_name in
        let pi_name = Resolved_name.ModuleName (current_module_name, pi_name) in
        let resolved_form = Resolved_form.create_symbol position pi_name in
        assert_resolves_to ~symbol_table:symbol_table resolved_form semantic_form
      );

    "resolve symbol from qualified common module">::
      (fun _ ->
        let position = Symbols.semantic_form.position in
        let qualified_name = Unresolved_name.QualifiedName (common_module_name, "pi") in
        let semantic_form = Semantic_form.create_symbol position qualified_name in
        let pi_name = Resolved_name.ModuleName (common_module_name, pi_name) in
        let resolved_form = Resolved_form.create_symbol position pi_name in
        assert_resolves_to ~symbol_table:symbol_table resolved_form semantic_form
      );

    "resolve function parameter that shadows module symbol">::
      (fun _ ->
        assert_resolves_to ~symbol_table:symbol_table Fns.resolved_form Fns.semantic_form
      );

    "resolve let binding that shadows module symbol">::
      (fun _ ->
        assert_resolves_to ~symbol_table:symbol_table Lets.resolved_form Lets.semantic_form
      );

      (*
    "resolve let binding that shadows function parameter">::
    "resolve undefined symbol">::
      *)
  ]
