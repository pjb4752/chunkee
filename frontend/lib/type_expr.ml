open Printf

type t =
  | SimpleType of Name_expr.t
  | CompoundType of t list

let rec inspect = function
  | SimpleType type_expr -> sprintf "SimpleType(%s)" @@ Name_expr.inspect type_expr
  | CompoundType type_exprs -> begin
    let type_exprs = List.map inspect type_exprs in
    sprintf "CompoundType([%s])" (String.concat "; " type_exprs)
  end
