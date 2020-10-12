open Frontend.Ast
open Frontend.Lexing

let form = Form.Symbol ({ line_num = 1; char_num = 1 }, "fat?", "fat?")

let parsed = Parsed_node.Symbol (BareName "fat?", { line_num = 1; char_num = 1 })
