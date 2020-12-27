open Frontend.Ast
open Frontend.Lexing

let source = "55"

let lexed_value = {
  Form.metadata = { line_num = 1; char_num = 1 };
  source = source;
  lexed = Form.Number 55.0
}

let parsed_value = Parsed_node.NumLit (
  55.0,
  { line_num = 1; char_num = 1 }
)
