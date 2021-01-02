open Frontend.Resolving
open OUnit2

module Identifier = Frontend.Identifier
module Intrinsics = Frontend.Intrinsics
module Module = Frontend.Module
module Module_name = Frontend.Module_name
module Name = Frontend.Name
module Name_expr = Frontend.Name_expr
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

let assert_resolves_to ?symbol_table:(symbol_table=symbol_table) expected parsed_node =
  assert_equal ~printer:Result.inspect (Ok expected) (resolve_node symbol_table parsed_node)

let suite =
  "Resolving suite">::: [
    "resolve number literal">::
      (fun _ ->
        assert_resolves_to Numbers.resolved_value Numbers.parsed_value
      );

    "resolve string literal">::
      (fun _ ->
        assert_resolves_to Strings.resolved_value Strings.parsed_value
      );

    "resolve symbol from common module">::
      (fun _ ->
        let resolved_value = {
          RNode.metadata = Symbols.parsed_value.metadata;
          parsed = RNode.Symbol (Name.Module (common_module_name, pi_name))
        } in
        assert_resolves_to ~symbol_table:symbol_table resolved_value Symbols.parsed_value
      );

    "resolve symbol from current module that shadows common module">::
      (fun _ ->
        let symbol_table = Symbol_table.define_var symbol_table pi_name Type.Bool in
        let resolved_value = {
          RNode.metadata = Symbols.parsed_value.metadata;
          parsed = RNode.Symbol (Name.Module (current_module_name, pi_name))
        } in
        assert_resolves_to ~symbol_table:symbol_table resolved_value Symbols.parsed_value
      );

    "resolve symbol from qualified current module">::
      (fun _ ->
        let symbol_table = Symbol_table.define_var symbol_table pi_name Type.Bool in
        let qualified_name = Name_expr.QualName (current_module_name, "pi") in
        let parsed_value = { Symbols.parsed_value with parsed = PNode.Symbol qualified_name } in
        let resolved_value = {
          RNode.metadata = Symbols.parsed_value.metadata;
          parsed = RNode.Symbol (Name.Module (current_module_name, pi_name))
        } in
        assert_resolves_to ~symbol_table:symbol_table resolved_value parsed_value
      );

    "resolve symbol from qualified common module">::
      (fun _ ->
        let qualified_name = Name_expr.QualName (common_module_name, "pi") in
        let parsed_value = { Symbols.parsed_value with parsed = PNode.Symbol qualified_name } in
        let resolved_value = {
          RNode.metadata = Symbols.parsed_value.metadata;
          parsed = RNode.Symbol (Name.Module (common_module_name, pi_name))
        } in
        assert_resolves_to ~symbol_table:symbol_table resolved_value parsed_value
      );

    "resolve function parameter that shadows module symbol">::
      (fun _ ->
        assert_resolves_to ~symbol_table:symbol_table Fns.resolved_value Fns.parsed_value
      );

    "resolve let binding that shadows module symbol">::
      (fun _ ->
        assert_resolves_to ~symbol_table:symbol_table Lets.resolved_value Lets.parsed_value
      );

      (*
    "resolve let binding that shadows function parameter">::
    "resolve undefined symbol">::
      *)
  ]
