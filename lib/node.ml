open Printf

module Param = Id

module Binding = struct
  module Name = Id
  type 'a t = {
    name: Name.t;
    expr: 'a;
  }

  let from_node name expr = { name; expr; }

  let to_string { name; expr; } expr_fn =
    sprintf "(%s %s)" (Name.to_string name) (expr_fn expr)

  let values { name; expr; } = (name, expr)
end

type 'a t =
  | NumLit of float
  | StrLit of string
  | SymLit of 'a
  | Def of (Module.Var.Name.t * 'a t)
  | Fn of (Param.t list * 'a t)
  | If of ('a t * 'a t * 'a t)
  | Let of ('a t Binding.t list * 'a t)
  | Apply of ('a t * 'a t list)

let to_string str_of_a node =
  let rec to_string' node =
    let string_of_def name expr =
      let name = Module.Var.Name.to_string name in
      sprintf "(def %s %s)" name (to_string' expr) in
    let string_of_fn params body =
      let params = String.concat " " (List.map Param.to_string params) in
      sprintf "(fn (params %s)) %s)" params (to_string' body) in
    let string_of_if test if_expr else_expr =
      let exprs = List.map to_string' [test; if_expr; else_expr] in
      sprintf "(if %s)" (String.concat " " exprs) in
    let string_of_binding binding =
      Binding.to_string binding to_string' in
    let string_of_let bindings body =
      let bindings = String.concat " " (List.map string_of_binding bindings) in
      sprintf "(let (bindings %s) %s)" bindings (to_string' body) in
    let string_of_apply fn args =
      let args = String.concat " " (List.map to_string' args) in
      sprintf "(apply %s %s)" (to_string' fn) args in
    match node with
    | NumLit n -> sprintf "(numlit %.2f)" n
    | StrLit s -> sprintf "(strlit %s)" s
    | SymLit a -> sprintf "(symlit %s)" (str_of_a a)
    | Def (name, expr) -> string_of_def name expr
    | Fn (params, body) -> string_of_fn params body
    | If (test, if_expr, else_expr) -> string_of_if test if_expr else_expr
    | Let (bindings, body) -> string_of_let bindings body
    | Apply (fn, args) -> string_of_apply fn args in
  to_string' node
