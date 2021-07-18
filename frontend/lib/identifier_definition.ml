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

module DeclaredTypes = Map.Make(String)

let module_variable_exists symbol_table name =
  let module_name = Module.name @@ Symbol_table.current_module symbol_table in
  Option.is_some @@ Symbol_table.find_variable symbol_table module_name name

let find_function_type symbol_table position parameters return_type =
  let param_types = List.map (fun param -> snd @@ Form.VarDef.to_tuple param) parameters in
  let compound_type = Type_expression.CompoundType (List.append param_types [return_type]) in
  match Symbol_table.resolve_type symbol_table compound_type with
  | Ok compound_type -> Ok compound_type
  | Error error -> begin
      Error (Compile_error.definition_errors position [
        Symbol_table.err_string error
      ])
  end

let find_variable_type symbol_table position expression =
  match expression with
  | Form.Number _ -> Ok Type.Number
  | Form.String _ -> Ok Type.String
  | Form.Fn { parameters; return_type; _ } -> find_function_type symbol_table position parameters return_type
  | _ -> assert false

let define_module_variables symbol_table = function
  | { Form.position; parsed = Form.Def { name; body_form } } -> begin
    if module_variable_exists symbol_table name then
      Error (Compile_error.definition_errors position [
        sprintf "var with name '%s' is already defined" name
      ])
    else (
      let* variable_type = find_variable_type symbol_table position body_form.parsed in
      return (Symbol_table.define_variable symbol_table name variable_type)
    )
  end
  | _ -> return symbol_table

let define_identifiers symbol_table form =
  define_module_variables symbol_table form
