module Semantic_form = Syntax_form.Make(Name_expr)(Type_expr)

module Resolved_form = Syntax_form.Make(Name)(Type)
