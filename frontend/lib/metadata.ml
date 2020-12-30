open Printf

type t = { line_num: int; char_num: int; source: string }

let inspect { line_num; char_num; source } =
  sprintf "{ line_num = %d; char_num = %d; source = %s }" line_num char_num source
