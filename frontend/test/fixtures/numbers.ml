open Frontend.Ast
open Frontend.Lexing
open Frontend.Metadata

let source = "55"

let metadata = { line_num = 1; char_num = 1; source }

let lexed_value = {
  Form.metadata = metadata;
  value = Form.Number 55.0
}

let parsed_value = {
  Parsed_node.metadata = metadata;
  parsed = Parsed_node.NumLit 55.0
}

let resolved_value = {
  Resolved_node.metadata = metadata;
  parsed = Resolved_node.NumLit 55.0
}
