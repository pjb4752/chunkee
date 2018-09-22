open Printf

module Name = Id

module TypeDef = struct
  type t =
    | StrType of string
    | FnType of t list

  let from_string s = StrType s

  let from_list l = FnType l

  let rec to_string t =
    let string_of_fn l =
      sprintf "[%s]" (String.concat " " (List.map to_string l)) in
    match t with
    | StrType s -> s
    | FnType l -> string_of_fn l
end

module VarDef = struct
  module Name = Id

  type 'b t = {
    name: Name.t;
    tipe: 'b;
  }

  let from_parts name tipe = { name; tipe; }

  let to_tuple { name; tipe; } = (name, tipe)

  let to_string string_fn { name; tipe; } =
    let name = Name.to_string name
    and tipe = string_fn tipe in
    sprintf "[%s %s]" name tipe
end

module Binding = struct
  module Name = Id
  type 'a t = {
    name: Name.t;
    expr: 'a;
  }

  let from_node name expr = { name; expr; }

  let to_string { name; expr; } expr_fn =
    sprintf "(%s %s)" (Name.to_string name) (expr_fn expr)

  let to_tuple { name; expr; } = (name, expr)
end

type ('a, 'b) t =
  | NumLit of float
  | StrLit of string
  | SymLit of 'a
  | Rec of (Name.t * 'b VarDef.t list)
  | Def of (Name.t * ('a, 'b) t)
  | Fn of ('b VarDef.t list * ('a, 'b) t)
  | If of (('a, 'b) t * ('a, 'b) t * ('a, 'b) t)
  | Let of (('a, 'b) t Binding.t list * ('a, 'b) t)
  | Apply of (('a, 'b) t * ('a, 'b) t list)
  | Cast of ('b * ('a, 'b) t)

let to_string str_of_a str_of_b node =
  let vardef_to_string = VarDef.to_string str_of_b in
  let rec to_string' node =
    let string_of_rec name fdefs =
      let fdefs = List.map vardef_to_string fdefs in
      let fdefs = String.concat " " fdefs in
      sprintf "(defrec %s (fields %s))" (Name.to_string name) fdefs in
    let string_of_def name expr =
      let name = Name.to_string name in
      sprintf "(def %s %s)" name (to_string' expr) in
    let string_of_fn params body =
      let params = String.concat " " (List.map vardef_to_string params) in
      sprintf "(fn (params %s) %s)" params (to_string' body) in
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
    let string_of_cast tipe expr =
      let tipe = str_of_b tipe in
      sprintf "(cast %s %s)" tipe (to_string' expr) in
    match node with
    | NumLit n -> sprintf "(numlit %.2f)" n
    | StrLit s -> sprintf "(strlit %s)" s
    | SymLit a -> sprintf "(symlit %s)" (str_of_a a)
    | Rec (name, fdefs) -> string_of_rec name fdefs
    | Def (name, expr) -> string_of_def name expr
    | Fn (params, body) -> string_of_fn params body
    | If (test, if_expr, else_expr) -> string_of_if test if_expr else_expr
    | Let (bindings, body) -> string_of_let bindings body
    | Apply (fn, args) -> string_of_apply fn args
    | Cast (tipe, expr) -> string_of_cast tipe expr in
  to_string' node
