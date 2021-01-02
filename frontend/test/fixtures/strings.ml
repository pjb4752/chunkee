open Frontend.Ast
open Frontend.Lexing
open Frontend.Metadata

let source = "\"hello\""

let metadata = { line_num = 1; char_num = 1; source }

let lexed_value = {
  Form.metadata = metadata;
  lexed = Form.String "hello"
}

let parsed_value = {
  Parsed_node.metadata = metadata;
  parsed = Parsed_node.StrLit "hello"
}

let resolved_value = {
  Resolved_node.metadata = metadata;
  parsed = Resolved_node.StrLit "hello"
}
