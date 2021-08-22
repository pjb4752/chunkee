open Frontend.Ast
open Frontend.Stream_position

module Source_form = Frontend.Source_form

let source_string = "55"

let position = { line_number = 1; char_number = 1 }

let source_form = Source_form.create_number position 55.0

let semantic_form = Semantic_form.create_number position 55.0

let resolved_form = Resolved_form.create_number position 55.0
