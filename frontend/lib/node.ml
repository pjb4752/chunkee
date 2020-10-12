open Printf

module type InspectableType = sig
  type t

  val inspect: t -> string
end

module type N = sig
  type name_expr_t
  type type_expr_t

  module Binding: sig
    type 'a t

    val from_node: Identifier.t -> 'a -> 'a t

    val name: 'a t -> Identifier.t

    val expr: 'a t -> 'a

    val to_tuple: 'a t -> Identifier.t * 'a
  end

  module VarDef: sig
    type t

    val from_parts: Identifier.t -> type_expr_t -> t

    val to_tuple: t -> Identifier.t * type_expr_t
  end

  type t =
    | NumLit of float * Metadata.t
    | StrLit of string * Metadata.t
    | Symbol of name_expr_t * Metadata.t
    | Type of name_expr_t * Metadata.t
    | Def of Identifier.t * t * Metadata.t
    | Fn of VarDef.t list * type_expr_t * t * Metadata.t
    | If of t * t * t * Metadata.t
    | Let of t Binding.t list * t * Metadata.t
    | Apply of t * t list * Metadata.t
    | Cons of type_expr_t * t Binding.t list * Metadata.t
    | Get of t * Identifier.t * Metadata.t
    | Set of t * Identifier.t * t * Metadata.t
    | Cast of type_expr_t * t * Metadata.t

  val inspect: t -> string
end

module Make (NameExpr: InspectableType) (TypeExpr: InspectableType) = struct
  type name_expr_t = NameExpr.t
  type type_expr_t = TypeExpr.t

  module Binding = struct
    type 'a t = {
      name: Identifier.t;
      expr: 'a;
    }

    let from_node name expr = { name; expr; }

    let name { name; _ } = name

    let expr { expr; _ } = expr

    let to_tuple { name; expr; } = (name, expr)

    let inspect inspect' { name; expr; } =
      sprintf "Binding(%s, %s)" (Identifier.inspect name) (inspect' expr)
  end

  module VarDef = struct
    type t = {
      name: Identifier.t;
      tipe: type_expr_t
    }

    let from_parts name tipe = { name; tipe; }

    let to_tuple { name; tipe; } = (name, tipe)

    let inspect { name; tipe; } =
      let name = Identifier.inspect name in
      let tipe = TypeExpr.inspect tipe in
      sprintf "VarDef({ name = %s; tipe = %s })" name tipe
  end

  type t =
    | NumLit of float * Metadata.t
    | StrLit of string * Metadata.t
    | Symbol of name_expr_t * Metadata.t
    | Type of name_expr_t * Metadata.t
    | Def of Identifier.t * t * Metadata.t
    | Fn of VarDef.t list * type_expr_t * t * Metadata.t
    | If of t * t * t * Metadata.t
    | Let of t Binding.t list * t * Metadata.t
    | Apply of t * t list * Metadata.t
    | Cons of type_expr_t * t Binding.t list * Metadata.t
    | Get of t * Identifier.t * Metadata.t
    | Set of t * Identifier.t * t * Metadata.t
    | Cast of type_expr_t * t * Metadata.t

  let inspect_numlit value metadata =
    sprintf "NumLit(%.2f, %s)" value @@ Metadata.inspect metadata

  let inspect_strlit value metadata =
    sprintf "StrLit(\"%s\", %s)" value @@ Metadata.inspect metadata

  let inspect_symbol value metadata =
    sprintf "Symbol(%s, %s)" (NameExpr.inspect value) @@ Metadata.inspect metadata

  let inspect_type value metadata =
    sprintf "Type(%s, %s)" (NameExpr.inspect value) @@ Metadata.inspect metadata

  let inspect_def inspect' name body metadata =
    sprintf "Def(%s, %s, %s)" (Identifier.inspect name) (inspect' body) @@ Metadata.inspect metadata

  let inspect_fn inspect' params return_type body metadata =
    let params = List.map VarDef.inspect params in
    let params = String.concat "; " params in
    let return_type = TypeExpr.inspect return_type in
    sprintf "Fn([%s], %s, %s, %s)" params return_type (inspect' body) @@ Metadata.inspect metadata

  let inspect_if inspect' test_expr if_expr else_expr metadata =
    let test_expr = inspect' test_expr in
    let if_expr = inspect' if_expr in
    let else_expr = inspect' else_expr in
    sprintf "If(%s, %s, %s, %s)" test_expr if_expr else_expr @@ Metadata.inspect metadata

  let inspect_let inspect' bindings body metadata =
    let bindings = List.map (Binding.inspect inspect') bindings in
    let bindings = String.concat "; " bindings in
    sprintf "Let([%s], %s, %s)" bindings (inspect' body) @@ Metadata.inspect metadata

  let inspect_apply inspect' fn arguments metadata =
    let arguments = List.map inspect' arguments in
    let arguments = String.concat "; " arguments in
    sprintf "Apply([%s], %s, %s)" (inspect' fn) arguments @@ Metadata.inspect metadata

  let inspect_cons inspect' cons_type bindings metadata =
    let bindings = List.map (Binding.inspect inspect') bindings in
    let bindings = String.concat "; " bindings in
    sprintf "Cons(%s, [%s], %s)" (TypeExpr.inspect cons_type) bindings @@ Metadata.inspect metadata

  let inspect_get inspect' target field metadata =
    sprintf "Get(%s, %s, %s)" (inspect' target) (Identifier.inspect field) @@ Metadata.inspect metadata

  let inspect_set inspect' target field expression metadata =
    let target = inspect' target in
    let field = Identifier.inspect field in
    let expression = inspect' expression in
    sprintf "Set(%s, %s, %s, %s)" target field expression @@ Metadata.inspect metadata

  let inspect_cast inspect' target_type expression metadata =
    let target_type = TypeExpr.inspect target_type in
    sprintf "Cast(%s, %s, %s)" target_type (inspect' expression) @@ Metadata.inspect metadata

  let rec inspect node =
    let inspect_def = inspect_def inspect in
    let inspect_fn = inspect_fn inspect in
    let inspect_if = inspect_if inspect in
    let inspect_let = inspect_let inspect in
    let inspect_apply = inspect_apply inspect in
    let inspect_cons = inspect_cons inspect in
    let inspect_get = inspect_get inspect in
    let inspect_set = inspect_set inspect in
    let inspect_cast = inspect_cast inspect in
    match node with
    | NumLit (value, metadata) -> inspect_numlit value metadata
    | StrLit (value, metadata) -> inspect_strlit value metadata
    | Symbol (value, metadata) -> inspect_symbol value metadata
    | Type (value, metadata) -> inspect_type value metadata
    | Def (name, body, metadata) -> inspect_def name body metadata
    | Fn (params, return_type, body, metadata) -> inspect_fn params return_type body metadata
    | If (test_expr, if_expr, else_expr, metadata) -> inspect_if test_expr if_expr else_expr metadata
    | Let (bindings, body, metadata) -> inspect_let bindings body metadata
    | Apply (fn, arguments, metadata) -> inspect_apply fn arguments metadata
    | Cons (cons_type, bindings, metadata) -> inspect_cons cons_type bindings metadata
    | Get (target, field, metadata) -> inspect_get target field metadata
    | Set (target, field, expression, metadata) -> inspect_set target field expression metadata
    | Cast (target_type, expression, metadata) -> inspect_cast target_type expression metadata
end
