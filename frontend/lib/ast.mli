module Semantic_form : Syntax_form.N with
  type name_t = Names.Unresolved_name.t and
  type type_t = Type_expr.t

module Resolved_form : Syntax_form.N with
  type name_t = Names.Resolved_name.t and
  type type_t = Type.t
