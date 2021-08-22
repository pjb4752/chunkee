open Frontend.Ast
open Frontend.Stream_position

module Source_form = Frontend.Source_form

let source_string = "(def x 5)"

let source_form =
  Source_form.create_list { line_number = 1; char_number = 1 } [
    Source_form.create_symbol { line_number = 1; char_number = 2 } "def";
    Source_form.create_symbol { line_number = 1; char_number = 6 } "x";
    Source_form.create_number { line_number = 1; char_number = 8 } 5.0
  ]

let semantic_form =
  Semantic_form.create_def { line_number = 1; char_number = 1 } "x" (
    Semantic_form.create_number { line_number = 1; char_number = 8 } 5.0
  )
