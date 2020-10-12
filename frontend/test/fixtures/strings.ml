open Frontend.Ast
open Frontend.Lexing

let form = Form.String ({ line_num = 1; char_num = 1 }, "\"hello\"", "hello")

let parsed = Parsed_node.StrLit ("hello", { line_num = 1; char_num = 1 })
