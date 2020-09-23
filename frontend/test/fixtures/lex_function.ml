open Frontend.Lex

let source = String.concat "\n" [
  "(def do-math (fn [[x num y num] num]";
  "  (let [first x";
  "        second y]";
  "    (+ first (- second 5)))))";
]

let make_number_form line_num char_num raw value =
  Form.Number ({ line_num; char_num }, raw, value)

let make_symbol_form line_num char_num value =
  Form.Symbol ({ line_num; char_num }, value, value)

let make_list_form line_num char_num raw value =
  Form.List ({ line_num; char_num }, raw, value)

let make_vector_form line_num char_num raw value =
  Form.Vector ({ line_num; char_num }, raw, value)

let def_name_form = make_symbol_form 1 2 "def"

let do_math_form = make_symbol_form 1 6 "do-math"

let fn_name_form = make_symbol_form 1 15 "fn"

let x_arg_form = make_symbol_form 1 20 "x"

let x_num_form = make_symbol_form 1 22 "num"

let y_arg_form = make_symbol_form 1 26 "y"

let y_num_form = make_symbol_form 1 28 "num"

let fn_args_form = make_vector_form 1 19 "[x num y num]" [x_arg_form; x_num_form; y_arg_form; y_num_form]

let fn_ret_form = make_symbol_form 1 33 "num"

let fn_spec_form = make_vector_form 1 18 "[[x num y num] num]" [fn_args_form; fn_ret_form]

let let_name_form = make_symbol_form 2 4 "let"

let let_first_form = make_symbol_form 2 9 "first"

let let_x_form = make_symbol_form 2 15 "x"

let let_second_form = make_symbol_form 3 9 "second"

let let_y_form = make_symbol_form 3 16 "y"

let let_binds_form = make_vector_form 2 8 "[first x\n        second y]"
  [let_first_form; let_x_form; let_second_form; let_y_form]

let plus_form = make_symbol_form 4 6 "+"

let first_form = make_symbol_form 4 8 "first"

let minus_form = make_symbol_form 4 15 "-"

let second_form = make_symbol_form 4 17 "second"

let five_form = make_number_form 4 24 "5" 5.00

let subtract_form = make_list_form 4 14 "(- second 5)" [minus_form; second_form; five_form]

let add_form = make_list_form 4 5 "(+ first (- second 5))" [plus_form; first_form; subtract_form]

let let_form = make_list_form 2 3 "(let [first x\n        second y]\n    (+ first (- second 5)))"
  [let_name_form; let_binds_form; add_form]

let fn_form = make_list_form 1 14
  "(fn [[x num y num] num]\n  (let [first x\n        second y]\n    (+ first (- second 5))))"
  [fn_name_form; fn_spec_form; let_form]

let form = make_list_form 1 1
  "(def do-math (fn [[x num y num] num]\n  (let [first x\n        second y]\n    (+ first (- second 5)))))"
  [def_name_form; do_math_form; fn_form]
