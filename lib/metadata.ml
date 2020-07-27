open Printf

type t = { line_num: int; char_num: int }

let debug_string { line_num; char_num } =
  sprintf "{ line_num = %d; char_num = %d }" line_num char_num
