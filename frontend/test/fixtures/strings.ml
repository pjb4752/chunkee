open Frontend.Ast
open Frontend.Lexing

let source = "\"hello\""

let lexed_value = {
  Form.metadata = { line_num = 1; char_num = 1 };
  source = source;
  lexed = Form.String "hello"
}

let parsed_value = Parsed_node.StrLit (
  "hello",
  { line_num = 1; char_num = 1 }
)
