open Frontend.Ast
open Frontend.Lexing
open Frontend.Metadata

let source = "pi"

let metadata = { line_num = 1; char_num = 1; source }

let lexed_value = {
  Form.metadata = metadata;
  lexed = Form.Symbol "pi"
}

let parsed_value = {
  Parsed_node.metadata = metadata;
  parsed = Parsed_node.Symbol (BareName "pi")
}
