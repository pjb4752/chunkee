open Lex_helpers

let source = "(def x ^(create-type))"

let def = make_symbol_form 1 2 "def"

let x = make_symbol_form 1 6 "x"

let create_type = make_symbol_form 1 10 "create-type"

let extension_expression = make_list_form 1 9 "(create-type)" [create_type]

let extension = make_extension_form 1 8 "^(create-type)" extension_expression

let form = make_list_form 1 1 "(def x ^(create-type))" [def; x; extension;]
