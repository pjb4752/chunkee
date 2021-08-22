open Frontend.Ast
open Frontend.Names
open Frontend.Stream_position

module Source_form = Frontend.Source_form

let source_string = "pi"

let position = { line_number = 1; char_number = 1 }

let source_form = Source_form.create_symbol position "pi"

let semantic_form = Semantic_form.create_symbol position (Unresolved_name.UnqualifiedName "pi")
