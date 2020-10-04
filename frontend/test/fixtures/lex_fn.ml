open Frontend.Lex

let source = "(fn [[x num] num] (+ x 5))"

let form = Form.List ({ line_num = 1; char_num = 1 }, "(fn [[x num] num] (+ x 5))", [
  Form.Symbol ({ line_num = 1; char_num = 2 }, "fn", "fn");
  Form.Vector ({ line_num = 1; char_num = 5 }, "[[x num] num]", [
    Form.Vector ({ line_num = 1; char_num = 6 }, "[x num]", [
      Form.Symbol ({ line_num = 1; char_num = 7 }, "x", "x");
      Form.Symbol ({ line_num = 1; char_num = 9 }, "num", "num")
    ]);
    Form.Symbol ({ line_num = 1; char_num = 14 }, "num", "num")
  ]);
  Form.List ({ line_num = 1; char_num = 19 }, "(+ x 5)", [
    Form.Symbol ({ line_num = 1; char_num = 20 }, "+", "+");
    Form.Symbol ({ line_num = 1; char_num = 22 }, "x", "x");
    Form.Number ({ line_num = 1; char_num = 24 }, "5", 5.0);
  ]);
])
