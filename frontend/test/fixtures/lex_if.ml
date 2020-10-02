open Lex_helpers

let source = String.concat "\n" [
  "(if true";
  "  (print \"hi\")";
  "  (print \"bye\"))";
]

let if_name_form = make_symbol_form 1 2 "if"

let true_form = make_symbol_form 1 5 "true"

let print_form = make_symbol_form 2 4 "print"

let hi_form = make_string_form 2 10 "hi"

let print_hi_form = make_list_form 2 3 "(print \"hi\")" [print_form; hi_form]

let print_form = make_symbol_form 3 4 "print"

let bye_form = make_string_form 3 10 "bye"

let print_bye_form = make_list_form 3 3 "(print \"bye\")" [print_form; bye_form]

let form = make_list_form 1 1
  "(if true\n  (print \"hi\")\n  (print \"bye\"))"
  [if_name_form; true_form; print_hi_form; print_bye_form]
