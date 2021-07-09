open Printf
open Common.Extensions
open Common.Extensions.Result
open Common.Extensions.Result.Syntax

module Form = Ast.Semantic_form

module Result = struct
  type t = (Symbol_table.t, Compile_error.t) result

  let inspect result =
    Result.inspect Symbol_table.inspect Compile_error.to_string result
end

module DeclaredTypes = Map.Make(Identifier)

let build_prefix { Metadata.line_num; char_num; _ } =
  sprintf "in expression at %d:%d" line_num char_num

let variable_exists symbol_table name =
  let module_name = Symbol_table.current_module symbol_table |> Module.name in
  Option.is_some @@ Symbol_table.module_var symbol_table module_name name

let find_function_type symbol_table metadata parameters return_type =
  let param_types = List.map (fun param -> snd @@ Form.VarDef.to_tuple param) parameters in
  let compound_type = Type_expr.CompoundType (List.append param_types [return_type]) in
  match Symbol_table.resolve_type symbol_table compound_type with
  | Ok compound_type -> Ok compound_type
  | Error error -> begin
      let prefix = build_prefix metadata in
      Error (Compile_error.definition_errors metadata prefix [
        Symbol_table.err_string error
      ])
  end

let find_variable_type symbol_table metadata expression =
  match expression with
  | Form.NumLit _ -> Ok Type.Number
  | Form.StrLit _ -> Ok Type.String
  | Form.Fn { parameters; return_type; _ } -> find_function_type symbol_table metadata parameters return_type
  | _ -> assert false

let define_variable symbol_table = function
  | { Form.metadata; parsed = Form.Def { name; body_form } } -> begin
    if variable_exists symbol_table name then
      let prefix = build_prefix metadata in
      Error (Compile_error.definition_errors metadata prefix [
        sprintf "var with name '%s' is already defined" @@ Identifier.to_string name
      ])
    else (
      let* variable_type = find_variable_type symbol_table metadata body_form.parsed in
      return (Symbol_table.define_var symbol_table name variable_type)
    )
  end
  | _ -> assert false

let define_identifiers symbol_table form =
  define_variable symbol_table form
