open Frontend.Lex

let source = "{ x 5 y \"hi\" }"

let form = Form.Record ({ line_num = 1; char_num = 1 }, "{ x 5 y \"hi\" }", [
  Form.Symbol ({ line_num = 1; char_num = 3 }, "x", "x");
  Form.Number ({ line_num = 1; char_num = 5 }, "5", 5.0);
  Form.Symbol ({ line_num = 1; char_num = 7 }, "y", "y");
  Form.String ({ line_num = 1; char_num = 9 }, "\"hi\"", "hi");
])
