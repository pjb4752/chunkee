open Lex_helpers

let source = String.concat "\n" [
  "(let [first x";
  "      second y]";
  "  (+ first second))";
]

let let_name_form = make_symbol_form 1 2 "let"

let let_first_form = make_symbol_form 1 7 "first"

let let_x_form = make_symbol_form 1 13 "x"

let let_second_form = make_symbol_form 2 7 "second"

let let_y_form = make_symbol_form 2 14 "y"

let let_binds_form = make_vector_form 1 6 "[first x\n      second y]"
  [let_first_form; let_x_form; let_second_form; let_y_form]

let plus_form = make_symbol_form 3 4 "+"

let first_form = make_symbol_form 3 6 "first"

let second_form = make_symbol_form 3 12 "second"

let add_form = make_list_form 3 3 "(+ first second)" [plus_form; first_form; second_form]

let form = make_list_form 1 1 "(let [first x\n      second y]\n  (+ first second))"
  [let_name_form; let_binds_form; add_form]
