open Frontend.Ast
open Frontend.Names
open Frontend.Stream_position

module Source_form = Frontend.Source_form

let source_string = String.concat "\n" [
  "(let (first 5";
  "      pi pi)";
  "  (+ first pi))"
]

let source_form =
  Source_form.create_list { line_number = 1; char_number = 1 } [
    Source_form.create_symbol { line_number = 1; char_number = 2 } "let";
    Source_form.create_list { line_number = 1; char_number = 6 } [
      Source_form.create_symbol { line_number = 1; char_number = 7 } "first";
      Source_form.create_number { line_number = 1; char_number = 13 } 5.0;
      Source_form.create_symbol { line_number = 2; char_number = 7 } "pi";
      Source_form.create_symbol { line_number = 2; char_number = 10 } "pi"
    ];
    Source_form.create_list { line_number = 3; char_number = 3 } [
      Source_form.create_symbol { line_number = 3; char_number = 4 } "+";
      Source_form.create_symbol { line_number = 3; char_number = 6 } "first";
      Source_form.create_symbol { line_number = 3; char_number = 12 } "pi"
    ]
  ]

let semantic_form =
  let pi_name = Unresolved_name.UnqualifiedName "pi" in
  let plus_name = Unresolved_name.UnqualifiedName "+" in
  let first_name = Unresolved_name.UnqualifiedName "first" in
  let bindings = [
    Semantic_form.Binding.create "first" (
      Semantic_form.create_number { line_number = 1; char_number = 13 } 5.0
    );
    Semantic_form.Binding.create "pi" (
      Semantic_form.create_symbol { line_number = 2; char_number = 10 } pi_name
    )
  ]
  in
  let body_form = Semantic_form.create_apply { line_number = 4; char_number = 3 }
    (Semantic_form.create_symbol { line_number = 3; char_number = 4 } plus_name)
    [
      Semantic_form.create_symbol { line_number = 3; char_number = 6 } first_name;
      Semantic_form.create_symbol { line_number = 3; char_number = 12 } pi_name
    ]
  in
  Semantic_form.create_let { line_number = 1; char_number = 1 } bindings body_form

let resolved_form =
  let pi_name = Resolved_name.ModuleName (Modules.Common.name, Modules.Common.pi_name) in
  let plus_name = Resolved_name.ModuleName (Modules.Common.name, Modules.Common.plus_name) in
  let first_name = Resolved_name.LocalName "first" in
  let local_pi_name = Resolved_name.LocalName "pi" in
  let bindings = [
    Resolved_form.Binding.create "first" (
      Resolved_form.create_number { line_number = 1; char_number = 13 } 5.0
    );
    Resolved_form.Binding.create "pi" (
      Resolved_form.create_symbol { line_number = 2; char_number = 10 } pi_name
    )
  ]
  in
  let body_form = Resolved_form.create_apply { line_number = 4; char_number = 3 }
    (Resolved_form.create_symbol { line_number = 3; char_number = 4 } plus_name)
    [
      Resolved_form.create_symbol { line_number = 3; char_number = 6 } first_name;
      Resolved_form.create_symbol { line_number = 3; char_number = 12 } local_pi_name
    ]
  in
  Resolved_form.create_let { line_number = 1; char_number = 1 } bindings body_form
