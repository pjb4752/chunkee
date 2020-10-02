open Frontend.Lex

let make_number_form line_num char_num raw value =
  Form.Number ({ line_num; char_num }, raw, value)

let make_string_form line_num char_num value =
  Form.String ({ line_num; char_num }, "\"" ^ value ^ "\"", value)

let make_symbol_form line_num char_num value =
  Form.Symbol ({ line_num; char_num }, value, value)

let make_list_form line_num char_num raw value =
  Form.List ({ line_num; char_num }, raw, value)

let make_vector_form line_num char_num raw value =
  Form.Vector ({ line_num; char_num }, raw, value)

let make_record_form line_num char_num raw value =
  Form.Record ({ line_num; char_num }, raw, value)

let make_extension_form line_num char_num raw value =
  Form.Extension ({ line_num; char_num }, raw, value)
