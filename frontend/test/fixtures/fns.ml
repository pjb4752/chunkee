open Frontend.Ast
open Frontend.Lexing
open Frontend.Metadata

module Identifier = Frontend.Identifier
module Name = Frontend.Name
module Type = Frontend.Type

let source = "(fn [[pi num] num] (+ pi 5))"

let metadata = { line_num = 1; char_num = 1; source }

let lexed_value = {
  Form.metadata = metadata;
  value = Form.List [
    {
      metadata = { line_num = 1; char_num = 2; source = "fn" };
      value = Form.Symbol "fn"
    };
    {
      metadata = { line_num = 1; char_num = 5; source = "[[pi num] num]" };
      value = Form.Vector [
        {
          metadata = { line_num = 1; char_num = 6; source = "[pi num]" };
          value = Form.Vector [
            {
              metadata = { line_num = 1; char_num = 7; source = "pi" };
              value = Form.Symbol "pi"
            };
            {
              metadata = { line_num = 1; char_num = 10; source = "num" };
              value = Form.Symbol "num"
            }
          ]
        };
        {
          metadata = { line_num = 1; char_num = 15; source = "num" };
          value = Form.Symbol "num"
        }
      ]
    };
    {
      metadata = { line_num = 1; char_num = 20; source = "(+ pi 5)" };
      value = Form.List [
        {
          metadata = { line_num = 1; char_num = 21; source = "+" };
          value = Form.Symbol "+"
        };
        {
          metadata = { line_num = 1; char_num = 23; source = "pi" };
          value = Form.Symbol "pi"
        };
        {
          metadata = { line_num = 1; char_num = 26; source = "5" };
          value = Form.Number 5.0
        }
      ]
    }
  ]
}

let parsed_value = {
  Parsed_node.metadata = metadata;
  parsed = Parsed_node.Fn {
    parameters = [
      Parsed_node.VarDef.from_parts (Identifier.from_string "pi") (SimpleType (BareName "num"));
    ];
    return_type = SimpleType (BareName "num");
    body_node = {
      metadata = { line_num = 1; char_num = 20; source = "(+ pi 5)" };
      parsed = Parsed_node.Apply {
        callable_node = {
          metadata = { line_num = 1; char_num = 21; source = "+" };
          parsed = Parsed_node.Symbol (BareName "+")
        };
        arguments = [
          {
            metadata = { line_num = 1; char_num = 23; source = "pi" };
            parsed = Parsed_node.Symbol (BareName "pi")
          };
          {
            metadata = { line_num = 1; char_num = 26; source = "5" };
            parsed = Parsed_node.NumLit 5.0
          }
        ]
      }
    }
  }
}

let resolved_value = {
  Resolved_node.metadata = metadata;
  parsed = Resolved_node.Fn {
    parameters = [
      Resolved_node.VarDef.from_parts (Identifier.from_string "pi") Type.Number;
    ];
    return_type = Type.Number;
    body_node = {
      metadata = { line_num = 1; char_num = 20; source = "(+ pi 5)" };
      parsed = Resolved_node.Apply {
        callable_node = {
          metadata = { line_num = 1; char_num = 21; source = "+" };
          parsed = Resolved_node.Symbol (
            Name.Module (Modules.Common.name, Modules.Common.plus_name)
          )
        };
        arguments = [
          {
            metadata = { line_num = 1; char_num = 23; source = "pi" };
            parsed = Resolved_node.Symbol (Name.Local "pi")
          };
          {
            metadata = { line_num = 1; char_num = 26; source = "5" };
            parsed = Resolved_node.NumLit 5.0
          }
        ]
      }
    }
  }
}
