open Printf
open Chunkee.Result

let repl_path0 = Chunkee.Module.Name.from_string "__repl__"
let repl_path = Chunkee.Module.Path.from_list [repl_path0]

let repl_name = Chunkee.Module.Name.from_string "main"
let repl_module = Chunkee.Module.from_parts repl_path repl_name

let print_list string_fn l =
  List.iter (fun i -> string_fn i |> (printf "%s\n")) l

let print_forms = print_list Chunkee.Lex.Form.to_string
let print_nodes = print_list (Chunkee.Node.to_string (fun s -> s))
let print_module modul = printf "%s\n" (Chunkee.Module.to_string modul)
let print_table table =
  printf "%s\n" (Chunkee.Table.to_string table)

let string_of_resolved = Chunkee.Node.to_string Chunkee.Name.to_string

let print_resolved = print_list string_of_resolved
let print_typechecked = print_list (fun (n, t) ->
  sprintf "%s:%s" (string_of_resolved n) (Chunkee.Type.to_string t))
let print_emitted = print_list (fun s -> s)

let print_result table nodes =
  print_typechecked nodes

let lex = Chunkee.Lex.lex
let parse = Chunkee.Parse.parse
let define = Chunkee.Resolve.define_vars
let resolve table modul nodes =
  let table = Chunkee.Table.update_module table modul in
  match Chunkee.Resolve.resolve table modul nodes with
  | Ok resolved -> Ok (table, resolved)
  | Error e -> Error e
let typecheck = Chunkee.Typecheck.check
let emit = Chunkee.Emit.emit

let eval table modul line =
  (lex line) >>= fun forms ->
  (parse forms) >>= fun nodes ->
  (define modul nodes) >>= fun modul ->
  (resolve table modul nodes) >>= fun (table, resolved) ->
  (typecheck table modul resolved) >>= fun typechecked ->
  return (table, modul, typechecked)

let main () =
  let rec loop table modul =
    let () = printf "-> " in
    let line = read_line () in
    if line = "(quit)" then printf "Goodbye\n"
    else begin
      match eval table modul line with
      | Ok (table, modul, typechecked)->
          let () = print_result table typechecked in
          let () = print_emitted (emit typechecked) in
          loop table modul
      | Error e ->
          let () = printf "%s\n" (Chunkee.Cmpl_err.to_string e) in
          loop table modul
    end in
  let table = Chunkee.Table.with_stdlib in
  let table = Chunkee.Table.insert_module table repl_module in
  loop table repl_module

let () = main ()
