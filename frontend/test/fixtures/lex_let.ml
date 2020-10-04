open Frontend.Lex

let source = String.concat "\n" [
  "(let [first x";
  "      second y]";
  "  (+ first second))";
]

let form = Form.List ({ line_num = 1; char_num = 1 }, "(let [first x\n      second y]\n  (+ first second))", [
  Form.Symbol ({ line_num = 1; char_num = 2 }, "let", "let");
  Form.Vector ({ line_num = 1; char_num = 6 }, "[first x\n      second y]", [
    Form.Symbol ({ line_num = 1; char_num = 7 }, "first", "first");
    Form.Symbol ({ line_num = 1; char_num = 13 }, "x", "x");
    Form.Symbol ({ line_num = 2; char_num = 7 }, "second", "second");
    Form.Symbol ({ line_num = 2; char_num = 14 }, "y", "y");
  ]);
  Form.List ({ line_num = 3; char_num = 3 }, "(+ first second)", [
    Form.Symbol ({ line_num = 3; char_num = 4 }, "+", "+");
    Form.Symbol ({ line_num = 3; char_num = 6 }, "first", "first");
    Form.Symbol ({ line_num = 3; char_num = 12 }, "second", "second");
  ]);
])
