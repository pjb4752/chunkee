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

  type t = {
    metadata: Metadata.t;
    parsed: u
  } and u =
    | NumLit of float
    | StrLit of string
    | Symbol of name_expr_t
    | Type of name_expr_t
    | Def of { name: Identifier.t; body_node: t }
    | Fn of { parameters: VarDef.t list; return_type: type_expr_t; body_node: t }
    | If of { test_node: t; if_node: t; else_node: t }
    | Let of { bindings: t Binding.t list; body_node: t }
    | Apply of { callable_node: t; arguments: t list }
    | Cons of { target_type: type_expr_t; bindings: t Binding.t list }
    | Get of { target_node: t; field: Identifier.t }
    | Set of { target_node: t; field: Identifier.t; body_node: t }
    | Cast of { target_type: type_expr_t; body_node: t }

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

  type t = {
    metadata: Metadata.t;
    parsed: u
  } and u =
    | NumLit of float
    | StrLit of string
    | Symbol of name_expr_t
    | Type of name_expr_t
    | Def of { name: Identifier.t; body_node: t }
    | Fn of { parameters: VarDef.t list; return_type: type_expr_t; body_node: t }
    | If of { test_node: t; if_node: t; else_node: t }
    | Let of { bindings: t Binding.t list; body_node: t }
    | Apply of { callable_node: t; arguments: t list }
    | Cons of { target_type: type_expr_t; bindings: t Binding.t list }
    | Get of { target_node: t; field: Identifier.t }
    | Set of { target_node: t; field: Identifier.t; body_node: t }
    | Cast of { target_type: type_expr_t; body_node: t }

  let inspect_node metadata parsed =
    sprintf "{ metadata = %s; parsed = %s }" (Metadata.inspect metadata) parsed

  let inspect_numlit value =
    sprintf "NumLit(%.2f)" value

  let inspect_strlit value =
    sprintf "StrLit(\"%s\")" value

  let inspect_symbol value =
    sprintf "Symbol(%s)" (NameExpr.inspect value)

  let inspect_type value =
    sprintf "Type(%s)" (NameExpr.inspect value)

  let inspect_def inspect' name body_node =
    sprintf "Def{ name = %s; body_node = %s }" (Identifier.inspect name) (inspect' body_node)

  let inspect_fn inspect' parameters return_type body_node =
    let parameters = List.map VarDef.inspect parameters in
    let parameters = String.concat "; " parameters in
    let return_type = TypeExpr.inspect return_type in
    let body_node = inspect' body_node in
    sprintf "Fn{ parameters = [%s]; return_type = %s; body_node = %s }" parameters return_type body_node

  let inspect_if inspect' test_node if_node else_node =
    let test_node = inspect' test_node in
    let if_node = inspect' if_node in
    let else_node = inspect' else_node in
    sprintf "If{ test_node = %s; if_node = %s; else_node = %s }" test_node if_node else_node

  let inspect_let inspect' bindings body_node =
    let bindings = List.map (Binding.inspect inspect') bindings in
    let bindings = String.concat "; " bindings in
    sprintf "Let{ bindings = [%s]; body_node = %s }" bindings (inspect' body_node)

  let inspect_apply inspect' callable_node arguments =
    let arguments = List.map inspect' arguments in
    let arguments = String.concat "; " arguments in
    sprintf "Apply{ callable_node = [%s]; arguments = %s }" (inspect' callable_node) arguments

  let inspect_cons inspect' target_type bindings =
    let bindings = List.map (Binding.inspect inspect') bindings in
    let bindings = String.concat "; " bindings in
    sprintf "Cons{ target_type = %s; bindings = [%s] }" (TypeExpr.inspect target_type) bindings

  let inspect_get inspect' target_node field =
    sprintf "Get{ target_node = %s; field = %s; }" (inspect' target_node) (Identifier.inspect field)

  let inspect_set inspect' target_node field body_node =
    let target_node = inspect' target_node in
    let field = Identifier.inspect field in
    let body_node = inspect' body_node in
    sprintf "Set{ target_node = %s; field = %s; body_node = %s }" target_node field body_node

  let inspect_cast inspect' target_type body =
    let target_type = TypeExpr.inspect target_type in
    sprintf "Cast{ target_type = %s; body = %s }" target_type (inspect' body)

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
    let inspected_node = match node.parsed with
    | NumLit value -> inspect_numlit value
    | StrLit value -> inspect_strlit value
    | Symbol value -> inspect_symbol value
    | Type value -> inspect_type value
    | Def { name; body_node } -> inspect_def name body_node
    | Fn { parameters; return_type; body_node } -> inspect_fn parameters return_type body_node
    | If { test_node; if_node; else_node } -> inspect_if test_node if_node else_node
    | Let { bindings; body_node } -> inspect_let bindings body_node
    | Apply { callable_node; arguments } -> inspect_apply callable_node arguments
    | Cons { target_type; bindings } -> inspect_cons target_type bindings
    | Get { target_node; field } -> inspect_get target_node field
    | Set { target_node; field; body_node } -> inspect_set target_node field body_node
    | Cast { target_type; body_node } -> inspect_cast target_type body_node
    in
    inspect_node node.metadata inspected_node
end
