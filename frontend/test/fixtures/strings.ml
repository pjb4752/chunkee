open Frontend.Lex
open Frontend.Parse

let form = Form.String ({ line_num = 1; char_num = 1 }, "\"hello\"", "hello")

let parsed = Node.StrLit ("hello", { line_num = 1; char_num = 1 })
