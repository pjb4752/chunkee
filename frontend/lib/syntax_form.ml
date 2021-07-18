open Printf

module type InspectableType = sig
  type t

  val inspect: t -> string
end

module type N = sig
  type name_t
  type type_t

  module Binding: sig
    type 'a t

    val from_form: string -> 'a -> 'a t

    val name: 'a t -> string

    val expr: 'a t -> 'a

    val to_tuple: 'a t -> string * 'a
  end

  module VarDef: sig
    type t

    val from_parts: string -> type_t -> t

    val to_tuple: t -> string * type_t
  end

  type t = {
    position: Stream_position.t;
    parsed: u
  } and u =
    | Number of float
    | String of string
    | Symbol of name_t
    | Type of name_t
    | Def of { name: string; body_form: t }
    | Fn of { parameters: VarDef.t list; return_type: type_t; body_form: t }
    | If of { test_form: t; if_form: t; else_form: t }
    | Let of { bindings: t Binding.t list; body_form: t }
    | Apply of { callable_form: t; arguments: t list }
    | Cons of { target_type: type_t; bindings: t Binding.t list }
    | Get of { target_form: t; field: string }
    | Set of { target_form: t; field: string; body_form: t }
    | Cast of { target_type: type_t; body_form: t }

  val inspect: t -> string
end

module Make (NameExpr: InspectableType) (TypeExpr: InspectableType) = struct
  type name_t = NameExpr.t
  type type_t = TypeExpr.t

  module Binding = struct
    type 'a t = {
      name: string;
      expr: 'a;
    }

    let from_form name expr = { name; expr; }

    let name { name; _ } = name

    let expr { expr; _ } = expr

    let to_tuple { name; expr; } = (name, expr)

    let inspect inspect' { name; expr; } =
      sprintf "Binding(%s, %s)" name (inspect' expr)
  end

  module VarDef = struct
    type t = {
      name: string;
      tipe: type_t
    }

    let from_parts name tipe = { name; tipe; }

    let to_tuple { name; tipe; } = (name, tipe)

    let inspect { name; tipe; } =
      let tipe = TypeExpr.inspect tipe in
      sprintf "VarDef({ name = %s; tipe = %s })" name tipe
  end

  type t = {
    position: Stream_position.t;
    parsed: u
  } and u =
    | Number of float
    | String of string
    | Symbol of name_t
    | Type of name_t
    | Def of { name: string; body_form: t }
    | Fn of { parameters: VarDef.t list; return_type: type_t; body_form: t }
    | If of { test_form: t; if_form: t; else_form: t }
    | Let of { bindings: t Binding.t list; body_form: t }
    | Apply of { callable_form: t; arguments: t list }
    | Cons of { target_type: type_t; bindings: t Binding.t list }
    | Get of { target_form: t; field: string }
    | Set of { target_form: t; field: string; body_form: t }
    | Cast of { target_type: type_t; body_form: t }

  let inspect_form position parsed =
    sprintf "{ position = %s; parsed = %s }" (Stream_position.inspect position) parsed

  let inspect_numlit value =
    sprintf "Number(%.2f)" value

  let inspect_strlit value =
    sprintf "String(\"%s\")" value

  let inspect_symbol value =
    sprintf "Symbol(%s)" (NameExpr.inspect value)

  let inspect_type value =
    sprintf "Type(%s)" (NameExpr.inspect value)

  let inspect_def inspect' name body_form =
    sprintf "Def{ name = %s; body_form = %s }" name (inspect' body_form)

  let inspect_fn inspect' parameters return_type body_form =
    let parameters = List.map VarDef.inspect parameters in
    let parameters = String.concat "; " parameters in
    let return_type = TypeExpr.inspect return_type in
    let body_form = inspect' body_form in
    sprintf "Fn{ parameters = [%s]; return_type = %s; body_form = %s }" parameters return_type body_form

  let inspect_if inspect' test_form if_form else_form =
    let test_form = inspect' test_form in
    let if_form = inspect' if_form in
    let else_form = inspect' else_form in
    sprintf "If{ test_form = %s; if_form = %s; else_form = %s }" test_form if_form else_form

  let inspect_let inspect' bindings body_form =
    let bindings = List.map (Binding.inspect inspect') bindings in
    let bindings = String.concat "; " bindings in
    sprintf "Let{ bindings = [%s]; body_form = %s }" bindings (inspect' body_form)

  let inspect_apply inspect' callable_form arguments =
    let arguments = List.map inspect' arguments in
    let arguments = String.concat "; " arguments in
    sprintf "Apply{ callable_form = [%s]; arguments = %s }" (inspect' callable_form) arguments

  let inspect_cons inspect' target_type bindings =
    let bindings = List.map (Binding.inspect inspect') bindings in
    let bindings = String.concat "; " bindings in
    sprintf "Cons{ target_type = %s; bindings = [%s] }" (TypeExpr.inspect target_type) bindings

  let inspect_get inspect' target_form field =
    sprintf "Get{ target_form = %s; field = %s; }" (inspect' target_form) field

  let inspect_set inspect' target_form field body_form =
    let target_form = inspect' target_form in
    let body_form = inspect' body_form in
    sprintf "Set{ target_form = %s; field = %s; body_form = %s }" target_form field body_form

  let inspect_cast inspect' target_type body =
    let target_type = TypeExpr.inspect target_type in
    sprintf "Cast{ target_type = %s; body = %s }" target_type (inspect' body)

  let rec inspect form =
    let inspect_def = inspect_def inspect in
    let inspect_fn = inspect_fn inspect in
    let inspect_if = inspect_if inspect in
    let inspect_let = inspect_let inspect in
    let inspect_apply = inspect_apply inspect in
    let inspect_cons = inspect_cons inspect in
    let inspect_get = inspect_get inspect in
    let inspect_set = inspect_set inspect in
    let inspect_cast = inspect_cast inspect in
    let inspected_form = match form.parsed with
    | Number value -> inspect_numlit value
    | String value -> inspect_strlit value
    | Symbol value -> inspect_symbol value
    | Type value -> inspect_type value
    | Def { name; body_form } -> inspect_def name body_form
    | Fn { parameters; return_type; body_form } -> inspect_fn parameters return_type body_form
    | If { test_form; if_form; else_form } -> inspect_if test_form if_form else_form
    | Let { bindings; body_form } -> inspect_let bindings body_form
    | Apply { callable_form; arguments } -> inspect_apply callable_form arguments
    | Cons { target_type; bindings } -> inspect_cons target_type bindings
    | Get { target_form; field } -> inspect_get target_form field
    | Set { target_form; field; body_form } -> inspect_set target_form field body_form
    | Cast { target_type; body_form } -> inspect_cast target_type body_form
    in
    inspect_form form.position inspected_form
end
