module Parsed_node : Node.N with
  type name_expr_t = Name_expr.t and
  type type_expr_t = Type_expr.t

module Resolved_node : Node.N with
  type name_expr_t = Name.t and
  type type_expr_t = Type.t
