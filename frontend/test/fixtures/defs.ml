open Frontend.Ast
open Frontend.Lexing
open Frontend.Metadata

let source = "(def x 5)"

let metadata = { line_num = 1; char_num = 1; source }

let lexed_value = {
  Form.metadata = metadata;
  lexed = Form.List [
    {
      metadata = { line_num = 1; char_num = 2; source = "def" };
      lexed = Form.Symbol "def"
    };
    {
      metadata = { line_num = 1; char_num = 6; source = "x" };
      lexed = Form.Symbol "x"
    };
    {
      metadata = { line_num = 1; char_num = 8; source = "5" };
      lexed = Form.Number 5.0
    }
  ]
}

let parsed_value = {
  Parsed_node.metadata = { line_num = 1; char_num = 1; source };
  parsed = Parsed_node.Def {
    name = Frontend.Identifier.from_string "x";
    body_node = {
      metadata = { line_num = 1; char_num = 8; source = "5" };
      parsed = Parsed_node.NumLit 5.0
    }
  }
}
