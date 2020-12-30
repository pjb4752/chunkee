open Frontend.Lexing

let source = "(def x ^(create-type))"

let lexed_value = {
  Form.metadata = { line_num = 1; char_num = 1; source = source };
  lexed = Form.List [
    {
      metadata = { line_num = 1; char_num = 2; source = "def" };
      lexed = Form.Symbol "def"
    };
    {
      metadata = { line_num = 1; char_num = 6; source = "x" };
      lexed = Form.Symbol "x"
    };
    {
      metadata = { line_num = 1; char_num = 8; source = "^(create-type)" };
      lexed = Form.Extension {
        metadata = { line_num = 1; char_num = 9; source = "(create-type)" };
        lexed = Form.List [
          {
            metadata = { line_num = 1; char_num = 10; source = "create-type" };
            lexed = Form.Symbol "create-type"
          }
        ]
      }
    }
  ]
}
