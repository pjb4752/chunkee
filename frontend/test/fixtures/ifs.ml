open Frontend.Lex
open Frontend.Parse

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

let parsed = Node.If (
  Node.Symbol (BareName "true", { line_num = 1; char_num = 5 }),
  Node.Apply (
    Node.Symbol (BareName "print", { line_num = 2; char_num = 4 }),
    [
      Node.StrLit ("hi", { line_num = 2; char_num = 10 });
    ],
    { line_num = 2; char_num = 4 }
  ),
  Node.Apply (
    Node.Symbol (BareName "print", { line_num = 3; char_num = 4 }),
    [
      Node.StrLit ("bye", { line_num = 3; char_num = 10 });
    ],
    { line_num = 3; char_num = 4 }
  ),
  { line_num = 1; char_num = 2 }
)
