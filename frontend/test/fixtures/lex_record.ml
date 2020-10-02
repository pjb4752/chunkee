open Lex_helpers

let source = "{ x 5 y \"hi\" }"

let x = make_symbol_form 1 3 "x"

let five = make_number_form 1 5 "5" 5.0

let y = make_symbol_form 1 7 "y"

let hi = make_string_form 1 9 "hi"

let form = make_record_form 1 1 "{ x 5 y \"hi\" }" [x; five; y; hi]
