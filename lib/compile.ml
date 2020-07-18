open Thwack.Extensions.Result
open Thwack.Extensions.Result.Syntax

module Node = Ast.Resolved_node

type t = (Symbol_table.t * (Node.t * Type.t) list, Cmpl_err.t) result

let lex = Lex.lex

let parse = Parse.parse

let declare = Declare.declare_toplevels

let resolve = Resolve.resolve

let typecheck = Typecheck.check

let compile_module symbol_table source =
  let* forms = lex source in
  let* nodes = parse forms in
  let* symbol_table = declare symbol_table nodes in
  let* nodes = resolve symbol_table nodes in
  let* nodes = typecheck symbol_table nodes in
  return (symbol_table, nodes)
