open Thwack.Result

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
  (typecheck table nodes) >>= fun (table, nodes) ->
  return (table, nodes)
