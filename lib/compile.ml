open Thwack.Result

let lex = Lex.lex

let parse = Parse.parse

let declare = Declare.declare_toplevels

let resolve = Resolve.resolve

let typecheck = Typecheck.check

let compile_module table modul source =
  (lex source) >>= fun forms ->
  (parse forms) >>= fun nodes ->
  (declare modul nodes) >>= fun modul ->
  (resolve table modul nodes) >>= fun (modul, nodes) ->
  (typecheck table modul nodes) >>= fun (modul, nodes) ->
  return (modul, nodes)
