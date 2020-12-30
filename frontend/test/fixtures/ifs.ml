open Frontend.Ast
open Frontend.Lexing
open Frontend.Metadata

let source = String.concat "\n" [
  "(if true";
  "  (print \"hi\")";
  "  (print \"bye\"))";
]

let metadata = { line_num = 1; char_num = 1; source }

let lexed_value = {
  Form.metadata = metadata;
  lexed = Form.List [
    {
      metadata = { line_num = 1; char_num = 2; source = "if" };
      lexed = Form.Symbol "if"
    };
    {
      metadata = { line_num = 1; char_num = 5; source = "true" };
      lexed = Form.Symbol "true"
    };
    {
      metadata = { line_num = 2; char_num = 3; source = "(print \"hi\")" };
      lexed = Form.List [
        {
          metadata = { line_num = 2; char_num = 4; source = "print" };
          lexed = Form.Symbol "print"
        };
        {
          metadata = { line_num = 2; char_num = 10; source = "\"hi\"" };
          lexed = Form.String "hi"
        }
      ]
    };
    {
      metadata = { line_num = 3; char_num = 3; source = "(print \"bye\")" };
      lexed = Form.List [
        {
          metadata = { line_num = 3; char_num = 4; source = "print" };
          lexed = Form.Symbol "print"
        };
        {
          metadata = { line_num = 3; char_num = 10; source = "\"bye\"" };
          lexed = Form.String "bye"
        }
      ]
    }
  ]
}

let parsed_value = {
  Parsed_node.metadata = metadata;
  parsed = Parsed_node.If {
    test_node = {
      metadata = { line_num = 1; char_num = 5; source = "true" };
      parsed = Parsed_node.Symbol (BareName "true")
    };
    if_node = {
      metadata = { line_num = 2; char_num = 3; source = "(print \"hi\")" };
      parsed = Parsed_node.Apply {
        callable_node = {
          metadata = { line_num = 2; char_num = 4; source = "print" };
          parsed = Parsed_node.Symbol (BareName "print")
        };
        arguments = [
          {
            metadata = { line_num = 2; char_num = 10; source = "\"hi\"" };
            parsed = Parsed_node.StrLit "hi"
          }
        ]
      }
    };
    else_node = {
      metadata = { line_num = 3; char_num = 3; source = "(print \"bye\")" };
      parsed = Parsed_node.Apply {
        callable_node = {
          metadata = { line_num = 3; char_num = 4; source = "print" };
          parsed = Parsed_node.Symbol (BareName "print")
        };
        arguments = [
          {
            metadata = { line_num = 3; char_num = 10; source = "\"bye\"" };
            parsed = Parsed_node.StrLit "bye"
          }
        ]
      }
    }
  }
}
