open Printf
open Common.Extensions.Result
open Common.Extensions.Result.Syntax

module Node = Ast.Parsed_node

type t = (Symbol_table.t, Cmpl_err.t) result

module DeclaredTypes = Map.Make(Identifier)

let build_prefix { Metadata.line_num; char_num } =
  sprintf "in expression at %d:%d" line_num char_num

let variable_exists symbol_table name =
  let module_name = Symbol_table.current_module symbol_table |> Module.name in
  Option.is_some @@ Symbol_table.module_var symbol_table module_name name

let find_function_type symbol_table parameters return_type metadata =
  let param_types = List.map (fun param -> snd @@ Node.VarDef.to_tuple param) parameters in
  let function_type = Type_expr.FnType (List.append param_types [return_type]) in
  match Symbol_table.resolve_type symbol_table function_type with
  | Ok function_type -> Ok function_type
  | Error error ->
      let prefix = build_prefix metadata in
      Error (Cmpl_err.definition_errors metadata prefix [
        Symbol_table.err_string error
      ])

let find_variable_type symbol_table expression metadata =
  match expression with
  | Node.NumLit _ -> Ok Type.Number
  | Node.StrLit _ -> Ok Type.String
  | Node.Fn (params, return_type, _, _) -> find_function_type symbol_table params return_type metadata
  (*| Node.Rec (fields, _) -> find_record_type symbol_table fields metadata*)
  | _ -> assert false

let define_variable symbol_table = function
  | Node.Def (name, expression, metadata) -> begin
    if variable_exists symbol_table name then
      let prefix = build_prefix metadata in
      Error (Cmpl_err.definition_errors metadata prefix [
        sprintf "var with name '%s' is already defined" @@ Identifier.to_string name
      ])
    else (
      let* variable_type = find_variable_type symbol_table expression metadata in
      return (Symbol_table.define_var symbol_table name variable_type)
    )
  end
  | _ -> assert false

let declare_type declared_types module_name = function
  | Node.Rec (_, metadata) ->
      if DeclaredTypes.mem name declared_types then
        let prefix = build_prefix metadata in
        Error (Cmpl_err.definition_errors metadata prefix [
          sprintf "type with name '%s' is already defined" @@ Identifier.to_string name
        ])
      else Ok (DeclaredTypes.add name (Record module_name) declared_types)
  | _ -> assert false

let declare_types module_name type_definitions =
  List.fold_right (fun type_definition declared_types ->
    let* declared_types = declared_types in
    let* declared_types = declare_type declared_types module_name type_definition in
    return declared_types) type_definitions (Ok DeclaredTypes.empty)

let resolve_type symbol_table declared_types type_expression metadata =
  let resolved_type = Symbol_table.resolve_type symbol_table ~lookup_fn:(Some (fun name ->
    match DeclaredTypes.find_opt name declared_types with
    | Some (Record module_name) -> Some (Type.Rec (module_name, name, []))
    | None -> None
    )) type_expression
  in
  match resolved_type with
  | Ok rt -> Ok rt
  | Error e ->
      let prefix = build_prefix metadata in
      Error (Cmpl_err.definition_errors metadata prefix [
        Symbol_table.err_string e
      ])

let resolve_record_constructor symbol_table declared_types fields metadata =
  let fold_fn field fields =
    let* fields = fields in
    let (name, type_expression) = Node.VarDef.to_tuple field in
    let name = Identifier.to_string name in
    let* resolved_type = resolve_type symbol_table declared_types type_expression metadata in
    return ((Identifier.from_string name, resolved_type) :: fields) in
  List.fold_right fold_fn fields (Ok [])

let define_record symbol_table declared_types name fields metadata =
  (resolve_record_constructor symbol_table declared_types fields metadata) >>= fun fields ->
  return (Symbol_table.define_record table name fields)

let define_type symbol_table declared_types = function
  | Node.Rec (name, fields, metadata) -> define_record symbol_table declared_types name fields metadata
  | _ -> assert false

let define_types symbol_table type_definitions =
  let module_name = Symbol_table.current_module symbol_table |> Module.name in
  match declare_types module_name type_definitions with
  | Error e -> Error e
  | Ok declare_types -> begin
    let fold_fn type_definition symbol_table =
      let* symbol_table = symbol_table in
      let* symbol_table = define_type symbol_table declared_types type_definition in
      return symbol_table in
    List.fold_right fold_fn type_definition (Ok symbol_table)
  end

let declare_node symbol_table node =
  declare_variable symbol_table node
