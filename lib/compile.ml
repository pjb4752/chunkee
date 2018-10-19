open Thwack.Result

module Node = Ast.Resolved_node

type t = (Symbol_table.t * (Node.t * Type.t) list, Cmpl_err.t) result

let lex = Lex.lex

let parse = Parse.parse

let declare = Declare.declare_toplevels

let resolve = Resolve.resolve

let typecheck = Typecheck.check

let compile_module table source =
  (lex source) >>= fun forms ->
  (parse forms) >>= fun nodes ->
  (declare table nodes) >>= fun table ->
  (resolve table nodes) >>= fun nodes ->
  (typecheck table nodes) >>= fun nodes ->
  return (table, nodes)
