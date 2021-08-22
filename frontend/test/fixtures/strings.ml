open Frontend.Ast
open Frontend.Stream_position

module Source_form = Frontend.Source_form

let source_string = "\"hello\""

let position = { line_number = 1; char_number = 1 }

let source_form = Source_form.create_string position "hello"

let semantic_form = Semantic_form.create_string position "hello"

let resolved_form = Resolved_form.create_string position "hello"
