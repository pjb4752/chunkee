open Frontend
open Frontend.Ast
open Frontend.Lexing
open Frontend.Metadata

let source = String.concat "\n" [
  "(let [first x";
  "      second y]";
  "  (+ first second))";
]

let metadata = { line_num = 1; char_num = 1; source }

let lexed_value = {
  Form.metadata = metadata;
  lexed = Form.List [
    {
      metadata = { line_num = 1; char_num = 2; source = "let" };
      lexed = Form.Symbol "let"
    };
    {
      metadata = { line_num = 1; char_num = 6; source = "[first x\n      second y]" };
      lexed = Form.Vector [
        {
          metadata = { line_num = 1; char_num = 7; source = "first" };
          lexed = Form.Symbol "first"
        };
        {
          metadata = { line_num = 1; char_num = 13; source = "x" };
          lexed = Form.Symbol "x"
        };
        {
          metadata = { line_num = 2; char_num = 7; source = "second" };
          lexed = Form.Symbol "second"
        };
        {
          metadata = { line_num = 2; char_num = 14; source = "y" };
          lexed = Form.Symbol "y"
        }
      ]
    };
    {
      metadata = { line_num = 3; char_num = 3; source = "(+ first second)" };
      lexed = Form.List [
        {
          metadata = { line_num = 3; char_num = 4; source = "+" };
          lexed = Form.Symbol "+"
        };
        {
          metadata = { line_num = 3; char_num = 6; source = "first" };
          lexed = Form.Symbol "first"
        };
        {
          metadata = { line_num = 3; char_num = 12; source = "second" };
          lexed = Form.Symbol "second"
        }
      ]
    }
  ]
}

let parsed_value = {
  Parsed_node.metadata = metadata;
  parsed = Parsed_node.Let {
    bindings = [
      Parsed_node.Binding.from_node (Identifier.from_string "first") {
        Parsed_node.metadata = { line_num = 1; char_num = 13; source = "x" };
        parsed = Parsed_node.Symbol (BareName "x")
      };
      Parsed_node.Binding.from_node (Identifier.from_string "second") {
        Parsed_node.metadata = { line_num = 2; char_num = 14; source = "y" };
        parsed = Parsed_node.Symbol (BareName "y")
      }
    ];
    body_node = {
      metadata = { line_num = 3; char_num = 3; source = "(+ first second)" };
      parsed = Parsed_node.Apply {
        callable_node = {
          metadata = { line_num = 3; char_num = 4; source = "+" };
          parsed = Parsed_node.Symbol (BareName "+")
        };
        arguments = [
          {
            metadata = { line_num = 3; char_num = 6; source = "first" };
            parsed = Parsed_node.Symbol (BareName "first")
          };
          {
            metadata = { line_num = 3; char_num = 12; source = "second" };
            parsed = Parsed_node.Symbol (BareName "second")
          }
        ]
      }
    }
  }
}
