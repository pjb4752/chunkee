open Frontend.Ast
open Frontend.Lexing

let source = String.concat "\n" [
  "(if true";
  "  (print \"hi\")";
  "  (print \"bye\"))";
]

let lexed_value = {
  Form.metadata = { line_num = 1; char_num = 1 };
  source = source;
  lexed = Form.List [
    {
      metadata = { line_num = 1; char_num = 2 };
      source = "if";
      lexed = Form.Symbol "if"
    };
    {
      metadata = { line_num = 1; char_num = 5 };
      source = "true";
      lexed = Form.Symbol "true"
    };
    {
      metadata = { line_num = 2; char_num = 3 };
      source = "(print \"hi\")";
      lexed = Form.List [
        {
          metadata = { line_num = 2; char_num = 4 };
          source = "print";
          lexed = Form.Symbol "print"
        };
        {
          metadata = { line_num = 2; char_num = 10 };
          source = "\"hi\"";
          lexed = Form.String "hi"
        }
      ]
    };
    {
      metadata = { line_num = 3; char_num = 3 };
      source = "(print \"bye\")";
      lexed = Form.List [
        {
          metadata = { line_num = 3; char_num = 4 };
          source = "print";
          lexed = Form.Symbol "print"
        };
        {
          metadata = { line_num = 3; char_num = 10 };
          source = "\"bye\"";
          lexed = Form.String "bye"
        }
      ]
    }
  ]
}

let parsed_value = Parsed_node.If (
  Parsed_node.Symbol (BareName "true", { line_num = 1; char_num = 5 }),
  Parsed_node.Apply (
    Parsed_node.Symbol (BareName "print", { line_num = 2; char_num = 4 }),
    [
      Parsed_node.StrLit ("hi", { line_num = 2; char_num = 10 });
    ],
    { line_num = 2; char_num = 4 }
  ),
  Parsed_node.Apply (
    Parsed_node.Symbol (BareName "print", { line_num = 3; char_num = 4 }),
    [
      Parsed_node.StrLit ("bye", { line_num = 3; char_num = 10 });
    ],
    { line_num = 3; char_num = 4 }
  ),
  { line_num = 1; char_num = 2 }
)
