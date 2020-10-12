open Frontend.Ast
open Frontend.Lexing

let form = Form.Number ({ line_num = 1; char_num = 1 }, "55", 55.0)

let parsed = Parsed_node.NumLit (55.0, { line_num = 1; char_num = 1 })
