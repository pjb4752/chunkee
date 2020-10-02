open Lex_helpers

let source = "(def x 5)"

let def_form = make_symbol_form 1 2 "def"

let x_form = make_symbol_form 1 6 "x"

let five_form = make_number_form 1 8 "5" 5.0

let form = make_list_form 1 1 "(def x 5)" [def_form; x_form; five_form]
