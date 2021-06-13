open Frontend.Ast
open Frontend.Lexing
open Frontend.Metadata

module Identifier = Frontend.Identifier
module Name = Frontend.Name

let source = String.concat "\n" [
  "(let [first 5";
  "      pi pi]";
  "  (+ first pi))";
]

let metadata = { line_num = 1; char_num = 1; source }

let lexed_value = {
  Form.metadata = metadata;
  value = Form.List [
    {
      metadata = { line_num = 1; char_num = 2; source = "let" };
      value = Form.Symbol "let"
    };
    {
      metadata = { line_num = 1; char_num = 6; source = "[first 5\n      pi pi]" };
      value = Form.Vector [
        {
          metadata = { line_num = 1; char_num = 7; source = "first" };
          value = Form.Symbol "first"
        };
        {
          metadata = { line_num = 1; char_num = 13; source = "5" };
          value = Form.Number 5.0
        };
        {
          metadata = { line_num = 2; char_num = 7; source = "pi" };
          value = Form.Symbol "pi"
        };
        {
          metadata = { line_num = 2; char_num = 10; source = "pi" };
          value = Form.Symbol "pi"
        }
      ]
    };
    {
      metadata = { line_num = 3; char_num = 3; source = "(+ first pi)" };
      value = Form.List [
        {
          metadata = { line_num = 3; char_num = 4; source = "+" };
          value = Form.Symbol "+"
        };
        {
          metadata = { line_num = 3; char_num = 6; source = "first" };
          value = Form.Symbol "first"
        };
        {
          metadata = { line_num = 3; char_num = 12; source = "pi" };
          value = Form.Symbol "pi"
        }
      ]
    }
  ]
}

let parsed_value = {
  Parsed_node.metadata = metadata;
  parsed = Parsed_node.Let {
    bindings = [
      Parsed_node.Binding.from_node (Identifier.from_string "first") {
        Parsed_node.metadata = { line_num = 1; char_num = 13; source = "5" };
        parsed = Parsed_node.NumLit 5.0
      };
      Parsed_node.Binding.from_node (Identifier.from_string "pi") {
        Parsed_node.metadata = { line_num = 2; char_num = 10; source = "pi" };
        parsed = Parsed_node.Symbol (BareName "pi")
      }
    ];
    body_node = {
      metadata = { line_num = 3; char_num = 3; source = "(+ first pi)" };
      parsed = Parsed_node.Apply {
        callable_node = {
          metadata = { line_num = 3; char_num = 4; source = "+" };
          parsed = Parsed_node.Symbol (BareName "+")
        };
        arguments = [
          {
            metadata = { line_num = 3; char_num = 6; source = "first" };
            parsed = Parsed_node.Symbol (BareName "first")
          };
          {
            metadata = { line_num = 3; char_num = 12; source = "pi" };
            parsed = Parsed_node.Symbol (BareName "pi")
          }
        ]
      }
    }
  }
}

let resolved_value = {
  Resolved_node.metadata = metadata;
  parsed = Resolved_node.Let {
    bindings = [
      Resolved_node.Binding.from_node (Identifier.from_string "first") {
        Resolved_node.metadata = { line_num = 1; char_num = 13; source = "5" };
        parsed = Resolved_node.NumLit 5.0
      };
      Resolved_node.Binding.from_node (Identifier.from_string "pi") {
        Resolved_node.metadata = { line_num = 2; char_num = 10; source = "pi" };
        parsed = Resolved_node.Symbol (
          Name.Module (Modules.Common.name, Modules.Common.pi_name)
        )
      }
    ];
    body_node = {
      metadata = { line_num = 3; char_num = 3; source = "(+ first pi)" };
      parsed = Resolved_node.Apply {
        callable_node = {
          metadata = { line_num = 3; char_num = 4; source = "+" };
          parsed = Resolved_node.Symbol (
            Name.Module (Modules.Common.name, Modules.Common.plus_name)
          )
        };
        arguments = [
          {
            metadata = { line_num = 3; char_num = 6; source = "first" };
            parsed = Resolved_node.Symbol (Name.Local "first")
          };
          {
            metadata = { line_num = 3; char_num = 12; source = "pi" };
            parsed = Resolved_node.Symbol (Name.Local "pi")
          }
        ]
      }
    }
  }
}
