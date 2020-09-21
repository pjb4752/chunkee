open Common.Extensions.Result
open Common.Extensions.Result.Syntax

module Node = Ast.Resolved_node

type t = (Symbol_table.t * Node.t list, Cmpl_err.t) result

let compile_form symbol_table form =
  let* parsed_node = Parse.parse_node form in
  let* symbol_table = Declare.define_node symbol_table parsed_node in
  let* resolved_node = Resolve.resolve_node symbol_table parsed_node in
  let* _ = Typecheck.check_node symbol_table resolved_node in
  return (symbol_table, resolved_node)

let compile_module symbol_table source =
  let* forms = Lex.lex source in
  let compile_nodes form accumulated_results =
    let* (symbol_table, final_nodes) = accumulated_results in
    let* (symbol_table, final_node) = compile_form symbol_table form in
    return (symbol_table, final_node :: final_nodes) in
  List.fold_right compile_nodes forms (Ok (symbol_table, []))
