open Lex_helpers

let source = "(fn [[x num] num] (+ x 5))"

let fn_name_form = make_symbol_form 1 2 "fn"

let x_arg_form = make_symbol_form 1 7 "x"

let x_num_form = make_symbol_form 1 9 "num"

let args_form = make_vector_form 1 6 "[x num]" [x_arg_form; x_num_form]

let ret_form = make_symbol_form 1 14 "num"

let spec_form = make_vector_form 1 5 "[[x num] num]" [args_form; ret_form]

let plus_form = make_symbol_form 1 20 "+"

let x_form = make_symbol_form 1 22 "x"

let five_form = make_number_form 1 24 "5" 5.0

let add_form = make_list_form 1 19 "(+ x 5)" [plus_form; x_form; five_form]

let form = make_list_form 1 1 "(fn [[x num] num] (+ x 5))" [fn_name_form; spec_form; add_form]
