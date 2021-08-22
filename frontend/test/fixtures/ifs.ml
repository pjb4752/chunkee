open Frontend.Ast
open Frontend.Names
open Frontend.Stream_position

module Source_form = Frontend.Source_form

let source_string = String.concat "\n" [
  "(if true";
  "  (print \"hi\")";
  "  (print \"bye\"))"
]

let source_form =
  Source_form.create_list { line_number = 1; char_number = 1 } [
    Source_form.create_symbol { line_number = 1; char_number = 2 } "if";
    Source_form.create_symbol { line_number = 1; char_number = 5 } "true";
    Source_form.create_list { line_number = 2; char_number = 3 } [
      Source_form.create_symbol { line_number = 2; char_number = 4 } "print";
      Source_form.create_string { line_number = 2; char_number = 10 } "hi"
    ];
    Source_form.create_list { line_number = 3; char_number = 3 } [
      Source_form.create_symbol { line_number = 3; char_number = 4 } "print";
      Source_form.create_string { line_number = 3; char_number = 10 } "bye"
    ]
  ]

let semantic_form =
  let true_name = Unresolved_name.UnqualifiedName "true" in
  let print_name = Unresolved_name.UnqualifiedName "print" in
  let test_form =
    Semantic_form.create_symbol { line_number = 1; char_number = 5 } true_name
  in
  let if_form =
    Semantic_form.create_apply
      { line_number = 2; char_number = 3 }
      (Semantic_form.create_symbol { line_number = 2; char_number = 4 } print_name)
      [Semantic_form.create_string { line_number = 2; char_number = 10 } "hi"]
  in
  let else_form =
    Semantic_form.create_apply
      { line_number = 3; char_number = 3 }
      (Semantic_form.create_symbol { line_number = 3; char_number = 4 } print_name)
      [Semantic_form.create_string { line_number = 3; char_number = 10 } "bye"]
  in Semantic_form.create_if { line_number = 1; char_number = 1 } test_form if_form else_form
