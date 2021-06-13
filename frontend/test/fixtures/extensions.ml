open Frontend.Lexing

let source = "(def x ^(create-type))"

let lexed_value = {
  Form.metadata = { line_num = 1; char_num = 1; source = source };
  value = Form.List [
    {
      metadata = { line_num = 1; char_num = 2; source = "def" };
      value = Form.Symbol "def"
    };
    {
      metadata = { line_num = 1; char_num = 6; source = "x" };
      value = Form.Symbol "x"
    };
    {
      metadata = { line_num = 1; char_num = 8; source = "^(create-type)" };
      value = Form.Extension {
        metadata = { line_num = 1; char_num = 9; source = "(create-type)" };
        value = Form.List [
          {
            metadata = { line_num = 1; char_num = 10; source = "create-type" };
            value = Form.Symbol "create-type"
          }
        ]
      }
    }
  ]
}
