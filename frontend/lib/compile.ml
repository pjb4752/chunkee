open Common.Extensions.Result
open Common.Extensions.Result.Syntax

type t = (Symbol_table.t * Ast.Resolved_node.t list, Cmpl_err.t) result

let compile_form symbol_table form =
  let* parsed_node = Parsing.parse_node form in
  let* symbol_table = Defining.define_for_node symbol_table parsed_node in
  let* resolved_node = Resolving.resolve_node symbol_table parsed_node in
  let* _ = Typecheck.typecheck_node symbol_table resolved_node in
  return (symbol_table, resolved_node)

let compile_module symbol_table source =
  let* forms = Lexing.lex source in
  let compile_nodes form accumulated_results =
    let* (symbol_table, final_nodes) = accumulated_results in
    let* (symbol_table, final_node) = compile_form symbol_table form in
    return (symbol_table, final_node :: final_nodes) in
  List.fold_right compile_nodes forms (Ok (symbol_table, []))
