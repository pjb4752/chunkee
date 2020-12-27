open Frontend.Ast
open Frontend.Lexing

let source = "fat?"

let lexed_value = {
  Form.metadata = { line_num = 1; char_num = 1 };
  source = source;
  lexed = Form.Symbol "fat?"
}

let parsed_value = Parsed_node.Symbol (
  BareName "fat?",
  { line_num = 1; char_num = 1 }
)
