module Var_ref = Id

module Parsed_node : Node.N with
  type name_expr_t = Var_ref.t and
  type type_expr_t = Type_ref.t

module Resolved_node : Node.N with
  type name_expr_t = Name.Var.t and
  type type_expr_t = Type.t
