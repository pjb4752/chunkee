open Frontend.Lex
open Frontend.Parse

let form = Form.Symbol ({ line_num = 1; char_num = 1 }, "fat?", "fat?")

let parsed = Node.Symbol (BareName "fat?", { line_num = 1; char_num = 1 })
