open Frontend
open Frontend.Ast
open Frontend.Lexing

let source = "(def x 5)"

let lexed_value = {
  Form.metadata = { line_num = 1; char_num = 1 };
  source = source;
  lexed = Form.List [
    {
      metadata = { line_num = 1; char_num = 2 };
      source = "def";
      lexed = Form.Symbol "def"
    };
    {
      metadata = { line_num = 1; char_num = 6 };
      source = "x";
      lexed = Form.Symbol "x"
    };
    {
      metadata = { line_num = 1; char_num = 8 };
      source = "5";
      lexed = Form.Number 5.0
    }
  ]
}

let parsed_value = Parsed_node.Def (
  Identifier.from_string "x",
  Parsed_node.NumLit (5.0, { line_num = 1; char_num = 8 }),
  { line_num = 1; char_num = 2 }
)
