open Frontend.Ast
open Frontend.Names
open Frontend.Stream_position

module Source_form = Frontend.Source_form
module Type = Frontend.Type
module Type_expression = Frontend.Type_expression


let source_string = String.concat "\n" [
  "(fn ((pi num) num)";
  "  (+ pi 5))"
]

let source_form =
  Source_form.create_list { line_number = 1; char_number = 1 } [
    Source_form.create_symbol { line_number = 1; char_number = 2 } "fn";
    Source_form.create_list { line_number = 1; char_number = 5 } [
      Source_form.create_list { line_number = 1; char_number = 6 } [
        Source_form.create_symbol { line_number = 1; char_number = 7 } "pi";
        Source_form.create_symbol { line_number = 1; char_number = 10 } "num";
      ];
      Source_form.create_symbol { line_number = 1; char_number = 15 } "num"
    ];
    Source_form.create_list { line_number = 2; char_number = 3 } [
      Source_form.create_symbol { line_number = 2; char_number = 4 } "+";
      Source_form.create_symbol { line_number = 2; char_number = 6 } "pi";
      Source_form.create_number { line_number = 2; char_number = 9 } 5.0
    ]
  ]

let semantic_form =
  let num_name = Unresolved_name.UnqualifiedName "num" in
  let plus_name = Unresolved_name.UnqualifiedName "+" in
  let pi_name = Unresolved_name.UnqualifiedName "pi" in
  let num_type = Type_expression.SimpleType num_name in
  let parameters = [
    Semantic_form.Parameter.create "pi" num_type
  ]
  in
  let body_form =
    Semantic_form.create_apply { line_number = 1; char_number = 20 }
      (Semantic_form.create_symbol { line_number = 1; char_number = 21 } plus_name)
      [
        Semantic_form.create_symbol { line_number = 1; char_number = 23 } pi_name;
        Semantic_form.create_number { line_number = 1; char_number = 26 } 5.0;
      ]
  in
  Semantic_form.create_fn { line_number = 1; char_number = 1 } parameters num_type body_form

let resolved_form =
  let plus_name = Resolved_name.ModuleName (Modules.Common.name, Modules.Common.plus_name) in
  let pi_name = Resolved_name.LocalName "pi" in
  let num_type = Type.Number in
  let parameters = [
      Resolved_form.Parameter.create "pi" Type.Number
  ]
  in
  let body_form =
    Resolved_form.create_apply { line_number = 1; char_number = 20 }
      (Resolved_form.create_symbol { line_number = 1; char_number = 21 } plus_name)
      [
        Resolved_form.create_symbol { line_number = 1; char_number = 23 } pi_name;
        Resolved_form.create_number { line_number = 1; char_number = 26 } 5.0;
      ]
  in
  Resolved_form.create_fn { line_number = 1; char_number = 1 } parameters num_type body_form
