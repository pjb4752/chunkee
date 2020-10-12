open Frontend.Lexing

let source = "(def x ^(create-type))"

let form = Form.List ({ line_num = 1; char_num = 1 }, "(def x ^(create-type))", [
  Form.Symbol ({ line_num = 1; char_num = 2 }, "def", "def");
  Form.Symbol ({ line_num = 1; char_num = 6 }, "x", "x");
  Form.Extension ({ line_num = 1; char_num = 8 }, "^(create-type)", (
    Form.List ({ line_num = 1; char_num = 9 }, "(create-type)", [
      Form.Symbol ({ line_num = 1; char_num = 10 }, "create-type", "create-type")
    ])
  ))
])
