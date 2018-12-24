open Printf

module type ShowableType = sig
  type t

  val to_string: t -> string
end

module type N = sig

  type name_expr_t

  type type_expr_t

  module Name = Id

  module Binding: sig

    module Name = Id

    type 'a t

    val from_node: Name.t -> 'a -> 'a t

    val name: 'a t -> Name.t

    val expr: 'a t -> 'a

    val to_tuple: 'a t -> Name.t * 'a

    val to_string: ('a -> string) -> 'a t -> string
  end

  module VarDef: sig

    module Name = Id

    type t

    val from_parts: Name.t -> type_expr_t -> t

    val to_tuple: t -> Name.t * type_expr_t

    val to_string: t -> string
  end

  type t =
    | NumLit of float * Metadata.t
    | StrLit of string * Metadata.t
    | SymLit of name_expr_t * Metadata.t
    | Rec of Name.t * VarDef.t list * Metadata.t
    | Def of Name.t * t * Metadata.t
    | Fn of VarDef.t list * type_expr_t * t * Metadata.t
    | If of t * t * t * Metadata.t
    | Let of t Binding.t list * t * Metadata.t
    | Apply of t * t list * Metadata.t
    | Cons of type_expr_t * t Binding.t list * Metadata.t
    | Get of t * Name.t * Metadata.t
    | Set of t * Name.t * t * Metadata.t
    | Cast of type_expr_t * t * Metadata.t

  val to_string: t -> string

end

module Make (NameExpr: ShowableType) (TypeExpr: ShowableType) = struct

  type name_expr_t = NameExpr.t

  type type_expr_t = TypeExpr.t

  module Name = Id

  module Binding = struct

    module Name = Id

    type 'a t = {
      name: Name.t;
      expr: 'a;
    }

    let from_node name expr = { name; expr; }

    let name { name; _ } = name

    let expr { expr; _ } = expr

    let to_tuple { name; expr; } = (name, expr)

    let to_string expr_to_string { name; expr; } =
      sprintf "(%s %s)" (Name.to_string name) (expr_to_string expr)
  end

  module VarDef = struct

    module Name = Id

    type t = {
      name: Name.t;
      tipe: type_expr_t
    }

    let from_parts name tipe = { name; tipe; }

    let to_tuple { name; tipe; } = (name, tipe)

    let to_string { name; tipe; } =
      let name = Name.to_string name in
      let tipe = TypeExpr.to_string tipe in
      sprintf "[%s %s]" name tipe
  end

  type t =
    | NumLit of float * Metadata.t
    | StrLit of string * Metadata.t
    | SymLit of name_expr_t * Metadata.t
    | Rec of Name.t * VarDef.t list * Metadata.t
    | Def of Name.t * t * Metadata.t
    | Fn of VarDef.t list * type_expr_t * t * Metadata.t
    | If of t * t * t * Metadata.t
    | Let of t Binding.t list * t * Metadata.t
    | Apply of t * t list * Metadata.t
    | Cons of type_expr_t * t Binding.t list * Metadata.t
    | Get of t * Name.t * Metadata.t
    | Set of t * Name.t * t * Metadata.t
    | Cast of type_expr_t * t * Metadata.t

let numlit_to_string numlit =
  sprintf "(numlit %.2f)" numlit

let strlit_to_string numlit =
  sprintf "(strlit \"%s\")" numlit

let symlit_to_string symlit =
  sprintf "(symlit %s)" (NameExpr.to_string symlit)

let rec_to_string name fields =
  let fields = List.map VarDef.to_string fields in
  let fields = String.concat " " fields in
  sprintf "(defrec %s (fields %s))" (Name.to_string name) fields

let def_to_string to_string' name expr =
  let name = Name.to_string name in
  sprintf "(def %s %s)" name (to_string' expr)

let fn_to_string to_string' params rtype body =
  let params = List.map VarDef.to_string params in
  let params = String.concat " " params in
  let rtype = TypeExpr.to_string rtype in
  sprintf "(fn (params %s) (rtype %s) %s)" params rtype (to_string' body)

let if_to_string to_string' test iff els =
  let exprs = List.map to_string' [test; iff; els] in
  sprintf "(if %s)" (String.concat " " exprs)

let let_to_string to_string' bindings body =
  let binding_to_string = Binding.to_string to_string' in
  let bindings = List.map binding_to_string bindings in
  let bindings = String.concat " " bindings in
  sprintf "(let (bindings %s) %s)" bindings (to_string' body)

let apply_to_string to_string' fn args =
  let args = String.concat " " (List.map to_string' args) in
  sprintf "(apply %s %s)" (to_string' fn) args

let cons_to_string to_string' tipe bindings =
  let binding_to_string = Binding.to_string to_string' in
  let bindings = List.map binding_to_string bindings in
  let bindings = String.concat " " bindings in
  sprintf "(recop %s %s)" (TypeExpr.to_string tipe) bindings

let get_to_string to_string' record field =
  let record = to_string' record in
  let field = Name.to_string field in
  sprintf "(get %s %s)" record field

let set_to_string to_string' record field expr =
  let record = to_string' record in
  let field = Name.to_string field in
  let expr = to_string' expr in
  sprintf "(set! %s %s %s)" record field expr

let cast_to_string to_string' tipe expr =
  let tipe = TypeExpr.to_string tipe in
  sprintf "(cast %s %s)" tipe (to_string' expr)

let rec to_string node =
  let def_to_string = def_to_string to_string in
  let fn_to_string = fn_to_string to_string in
  let if_to_string = if_to_string to_string in
  let let_to_string = let_to_string to_string in
  let apply_to_string = apply_to_string to_string in
  let cons_to_string = cons_to_string to_string in
  let get_to_string = get_to_string to_string in
  let set_to_string = set_to_string to_string in
  let cast_to_string = cast_to_string to_string in
  match node with
  | NumLit (num, _) -> numlit_to_string num
  | StrLit (str, _) -> strlit_to_string str
  | SymLit (sym, _) -> symlit_to_string sym
  | Rec (name, fields, _) -> rec_to_string name fields
  | Def (name, expr, _) -> def_to_string name expr
  | Fn (params, rtype, body, _) -> fn_to_string params rtype body
  | If (test, iff, els, _) -> if_to_string test iff els
  | Let (bindings, body, _) -> let_to_string bindings body
  | Apply (fn, args, _) -> apply_to_string fn args
  | Cons (tipe, bindings, _) -> cons_to_string tipe bindings
  | Get (record, field, _) -> get_to_string record field
  | Set (record, field, expr, _) -> set_to_string record field expr
  | Cast (tipe, expr, _) -> cast_to_string tipe expr
end
