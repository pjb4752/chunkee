open Printf
open Common.Extensions
open Common.Extensions.Result
open Common.Extensions.Result.Syntax

module Node = Ast.Parsed_node

module Result = struct
  type t = (Symbol_table.t, Cmpl_err.t) result

  let inspect result =
    Result.inspect result Symbol_table.inspect Cmpl_err.to_string
end

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
  | Error error -> begin
      let prefix = build_prefix metadata in
      Error (Cmpl_err.definition_errors metadata prefix [
        Symbol_table.err_string error
      ])
  end

let find_variable_type symbol_table expression metadata =
  match expression with
  | Node.NumLit _ -> Ok Type.Number
  | Node.StrLit _ -> Ok Type.String
  | Node.Fn (params, return_type, _, _) -> find_function_type symbol_table params return_type metadata
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

let define_node symbol_table node =
  define_variable symbol_table node
