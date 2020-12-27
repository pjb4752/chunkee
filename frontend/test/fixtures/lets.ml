open Frontend
open Frontend.Lexing
open Frontend.Ast

let source = String.concat "\n" [
  "(let [first x";
  "      second y]";
  "  (+ first second))";
]

let lexed_value = {
  Form.metadata = { line_num = 1; char_num = 1 };
  source = source;
  lexed = Form.List [
    {
      metadata = { line_num = 1; char_num = 2 };
      source = "let";
      lexed = Form.Symbol "let"
    };
    {
      metadata = { line_num = 1; char_num = 6 };
      source = "[first x\n      second y]";
      lexed = Form.Vector [
        {
          metadata = { line_num = 1; char_num = 7 };
          source = "first";
          lexed = Form.Symbol "first"
        };
        {
          metadata = { line_num = 1; char_num = 13 };
          source = "x";
          lexed = Form.Symbol "x"
        };
        {
          metadata = { line_num = 2; char_num = 7 };
          source = "second";
          lexed = Form.Symbol "second"
        };
        {
          metadata = { line_num = 2; char_num = 14 };
          source = "y";
          lexed = Form.Symbol "y"
        }
      ]
    };
    {
      metadata = { line_num = 3; char_num = 3 };
      source = "(+ first second)";
      lexed = Form.List [
        {
          metadata = { line_num = 3; char_num = 4 };
          source = "+";
          lexed = Form.Symbol "+"
        };
        {
          metadata = { line_num = 3; char_num = 6 };
          source = "first";
          lexed = Form.Symbol "first"
        };
        {
          metadata = { line_num = 3; char_num = 12 };
          source = "second";
          lexed = Form.Symbol "second"
        }
      ]
    }
  ]
}

let parsed_value = Parsed_node.Let (
  [
    Parsed_node.Binding.from_node
      (Identifier.from_string "first")
      (Parsed_node.Symbol (BareName "x", { line_num = 1; char_num = 13 }));
    Parsed_node.Binding.from_node
      (Identifier.from_string "second")
      (Parsed_node.Symbol (BareName "y", { line_num = 2; char_num = 14 }));
  ],
  Parsed_node.Apply (
    Parsed_node.Symbol (BareName "+", { line_num = 3; char_num = 4 }),
    [
      Parsed_node.Symbol (BareName "first", { line_num = 3; char_num = 6 });
      Parsed_node.Symbol (BareName "second", { line_num = 3; char_num = 12 });
    ],
    { line_num = 3; char_num = 4 }
  ),
  { line_num = 1; char_num = 2 }
)
