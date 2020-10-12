open Frontend
open Frontend.Lex
open Frontend.Parse

let source = "(def x 5)"

let form = Form.List ({ line_num = 1; char_num = 1 }, "(def x 5)", [
  Form.Symbol ({ line_num = 1; char_num = 2 }, "def", "def");
  Form.Symbol ({ line_num = 1; char_num = 6 }, "x", "x");
  Form.Number ({ line_num = 1; char_num = 8 }, "5", 5.0);
])

let parsed = Node.Def (
  Identifier.from_string "x",
  Node.NumLit (5.0, { line_num = 1; char_num = 8 }),
  { line_num = 1; char_num = 2 }
)