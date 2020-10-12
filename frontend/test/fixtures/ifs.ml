open Frontend.Ast
open Frontend.Lexing

let source = String.concat "\n" [
  "(if true";
  "  (print \"hi\")";
  "  (print \"bye\"))";
]

let form = Form.List ({ line_num = 1; char_num = 1 }, "(if true\n  (print \"hi\")\n  (print \"bye\"))", [
  Form.Symbol ({ line_num = 1; char_num = 2 }, "if", "if");
  Form.Symbol ({ line_num = 1; char_num = 5 }, "true", "true");
  Form.List ({ line_num = 2; char_num = 3 }, "(print \"hi\")", [
    Form.Symbol ({ line_num = 2; char_num = 4 }, "print", "print");
    Form.String ({ line_num = 2; char_num = 10 }, "\"hi\"", "hi");
  ]);
  Form.List ({ line_num = 3; char_num = 3 }, "(print \"bye\")", [
    Form.Symbol ({ line_num = 3; char_num = 4 }, "print", "print");
    Form.String ({ line_num = 3; char_num = 10 }, "\"bye\"", "bye");
  ]);
])

let parsed = Parsed_node.If (
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
