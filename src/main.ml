open Printf
open Result
open Table

let repl_path0 = Module.Name.from_string "__repl__"
let repl_path = Module.Path.from_list [repl_path0]

let repl_name = Module.Name.from_string "main"
let repl_module = Module.from_parts repl_path repl_name

let print_list string_fn l =
  List.iter (fun i -> string_fn i |> (printf "%s\n")) l

let print_forms = print_list Lex.Form.to_string
let print_nodes = print_list (Node.to_string (fun s -> s))
let print_module modul = printf "%s\n" (Module.to_string modul)
let print_table table =
  printf "%s\n" (Table.to_string table)

let string_of_resolved = Node.to_string Name.to_string

let print_resolved = print_list string_of_resolved
let print_typechecked = print_list (fun (n, t) ->
  sprintf "%s:%s" (string_of_resolved n) (Type.to_string t))
let print_emitted = print_list (fun s -> s)

let print_result table nodes =
  print_typechecked nodes

let lex = Lex.lex
let parse = Parse.parse
let define = Resolve.define_vars
let resolve table modul nodes =
  let table = Table.update_module table modul in
  match Resolve.resolve table modul nodes with
  | Ok resolved -> Ok (table, resolved)
  | Error e -> Error e
let typecheck = Typecheck.check
let emit = Emit.emit

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
          let () = printf "%s\n" (Cmpl_err.to_string e) in
          loop table modul
    end in
  let table = Table.with_stdlib in
  let table = Table.insert_module table repl_module in
  loop table repl_module
