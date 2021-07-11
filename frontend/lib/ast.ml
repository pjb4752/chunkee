module Semantic_form = Syntax_form.Make(Names.Unresolved_name)(Type_expression)

module Resolved_form = Syntax_form.Make(Names.Resolved_name)(Type)
