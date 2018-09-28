module Parsed_node = Node.Make(Name_expr)(Type_ref)

module Resolved_node = Node.Make(Name.Var)(Type)
