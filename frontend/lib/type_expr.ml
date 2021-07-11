open Printf

open Names

type t =
  | SimpleType of Unresolved_name.t
  | CompoundType of t list

let rec inspect = function
  | SimpleType type_expr -> sprintf "SimpleType(%s)" @@ Unresolved_name.inspect type_expr
  | CompoundType type_exprs -> begin
    let type_exprs = List.map inspect type_exprs in
    sprintf "CompoundType([%s])" (String.concat "; " type_exprs)
  end
