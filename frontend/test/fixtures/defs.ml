open Frontend
open Frontend.Ast
open Frontend.Lexing

let source = "(def x 5)"

let form = Form.List ({ line_num = 1; char_num = 1 }, "(def x 5)", [
  Form.Symbol ({ line_num = 1; char_num = 2 }, "def", "def");
  Form.Symbol ({ line_num = 1; char_num = 6 }, "x", "x");
  Form.Number ({ line_num = 1; char_num = 8 }, "5", 5.0);
])

let parsed = Parsed_node.Def (
  Identifier.from_string "x",
  Parsed_node.NumLit (5.0, { line_num = 1; char_num = 8 }),
  { line_num = 1; char_num = 2 }
)
