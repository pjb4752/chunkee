open Extensions
open Printf

type elt = { value: char; line_num: int; char_num: int }

type t = elt list

let prepend_line initial_list line_num chars =
  let total_chars = List.length chars in
  List.fold_left (fun (read_list, char_num) chr ->
    let element = { value = chr; line_num; char_num } in
    (element :: read_list, char_num - 1)
  ) (initial_list, total_chars) chars |> fst

let from_string str =
  let lines = List.rev @@ String.split_on_char '\n' str in
  let total_lines = List.length lines in
  List.fold_left (fun (read_list, line_num) line ->
    let chars = List.rev @@ String.to_chars line in
    let chars = if line_num = total_lines then chars else '\n' :: chars in
    (prepend_line read_list line_num chars, line_num - 1)
  ) ([], total_lines) lines |> fst

let elt_debug_string { value; line_num; char_num } =
  sprintf
    "{ value = %c;\n  line_num = %d;\n  char_num = %d; }"
    value line_num char_num

let debug_string read_list =
  let elts = List.map elt_debug_string read_list in
  sprintf "[%s]" @@ String.concat "\n" elts
