module Var_ref = Id

module Parsed_node = Node.Make(Var_ref)(Type_ref)

module Resolved_node = Node.Make(Name.Var)(Type)
