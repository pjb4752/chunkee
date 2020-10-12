open Frontend.Lex
open Frontend.Parse

let form = Form.Number ({ line_num = 1; char_num = 1 }, "55", 55.0)

let parsed = Node.NumLit (55.0, { line_num = 1; char_num = 1 })
