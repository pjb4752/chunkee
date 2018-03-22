open Printf

module Param = Id

module Binding = struct
  module Name = Id
  type 'a t = {
    name: Name.t;
    expr: 'a;
  }

  let from_node name expr = { name; expr; }
end

type t =
  | NumLit of float
  | StrLit of string
  | SymLit of string
  | Def of (Module.Var.Name.t * t)
  | Fn of (Param.t list * t)
  | If of (t * t * t)
  | Let of (t Binding.t list * t)
  | Apply of (t * t list)

let rec to_string node =
  let string_of_def name expr =
    let name = Module.Var.Name.to_string name in
    sprintf "(def %s %s)" name (to_string expr) in
  let string_of_fn params body =
    let params = String.concat " " (List.map Param.to_string params) in
    sprintf "(fn (params %s)) %s)" params (to_string body) in
  let string_of_if test if_expr else_expr =
    let exprs = List.map to_string [test; if_expr; else_expr] in
    sprintf "(if %s)" (String.concat " " exprs) in
  let string_of_binding (binding: t Binding.t) =
    let name = Binding.Name.to_string binding.name in
    sprintf "(%s %s)" name (to_string binding.expr) in
  let string_of_let bindings body =
    let bindings = String.concat " " (List.map string_of_binding bindings) in
    sprintf "(let (bindings %s) %s)" bindings (to_string body) in
  let string_of_apply fn args =
    let args = String.concat " " (List.map to_string args) in
    sprintf "(apply %s %s)" (to_string fn) args in
  match node with
  | NumLit n -> sprintf "(numlit %.2f)" n
  | StrLit s -> sprintf "(strlit %s)" s
  | SymLit s -> sprintf "(symlit %s)" s
  | Def (name, expr) -> string_of_def name expr
  | Fn (params, body) -> string_of_fn params body
  | If (test, if_expr, else_expr) -> string_of_if test if_expr else_expr
  | Let (bindings, body) -> string_of_let bindings body
  | Apply (fn, args) -> string_of_apply fn args
