open Frontend
open Frontend.Ast
open Frontend.Lexing
open Frontend.Metadata

let source = "(fn [[x num] num] (+ x 5))"

let metadata = { line_num = 1; char_num = 1; source }

let lexed_value = {
  Form.metadata = metadata;
  lexed = Form.List [
    {
      metadata = { line_num = 1; char_num = 2; source = "fn" };
      lexed = Form.Symbol "fn"
    };
    {
      metadata = { line_num = 1; char_num = 5; source = "[[x num] num]" };
      lexed = Form.Vector [
        {
          metadata = { line_num = 1; char_num = 6; source = "[x num]" };
          lexed = Form.Vector [
            {
              metadata = { line_num = 1; char_num = 7; source = "x" };
              lexed = Form.Symbol "x"
            };
            {
              metadata = { line_num = 1; char_num = 9; source = "num" };
              lexed = Form.Symbol "num"
            }
          ]
        };
        {
          metadata = { line_num = 1; char_num = 14; source = "num" };
          lexed = Form.Symbol "num"
        }
      ]
    };
    {
      metadata = { line_num = 1; char_num = 19; source = "(+ x 5)" };
      lexed = Form.List [
        {
          metadata = { line_num = 1; char_num = 20; source = "+" };
          lexed = Form.Symbol "+"
        };
        {
          metadata = { line_num = 1; char_num = 22; source = "x" };
          lexed = Form.Symbol "x"
        };
        {
          metadata = { line_num = 1; char_num = 24; source = "5" };
          lexed = Form.Number 5.0
        }
      ]
    }
  ]
}

let parsed_value = {
  Parsed_node.metadata = metadata;
  parsed = Parsed_node.Fn {
    parameters = [
      Parsed_node.VarDef.from_parts (Identifier.from_string "x") (SimpleType (BareName "num"));
    ];
    return_type = SimpleType (BareName "num");
    body_node = {
      metadata = { line_num = 1; char_num = 19; source = "(+ x 5)" };
      parsed = Parsed_node.Apply {
        callable_node = {
          metadata = { line_num = 1; char_num = 20; source = "+" };
          parsed = Parsed_node.Symbol (BareName "+")
        };
        arguments = [
          {
            metadata = { line_num = 1; char_num = 22; source = "x" };
            parsed = Parsed_node.Symbol (BareName "x")
          };
          {
            metadata = { line_num = 1; char_num = 24; source = "5" };
            parsed = Parsed_node.NumLit 5.0
          }
        ]
      }
    }
  }
}
