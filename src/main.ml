open Printf
open Result

let print_list string_fn l =
  List.iter (fun i -> string_fn i |> (printf "%s\n")) l

let print_forms = print_list Lex.Form.to_string
let print_nodes = print_list Node.to_string
let print_module modul = printf "%s\n" (Module.to_string modul)

let print_result modul nodes =
  let () = print_nodes nodes in
  print_module modul

let lex = Lex.lex
let parse = Parse.parse
let define = Resolve.define_vars

let eval repl_mod line =
  (lex line) >>= fun forms ->
  (parse forms) >>= fun nodes ->
  (define repl_mod nodes) >>= fun modul ->
  return (modul, nodes)

let main () =
  let rec loop modul =
    let () = printf "-> " in
    let line = read_line () in
    if line = "(quit)" then printf "Goodbye\n"
    else begin
      match eval modul line with
      | Ok (modul, nodes)->
          let () = print_result modul nodes in
          loop modul
      | Error e ->
          let () = printf "%s\n" (Cmpl_err.to_string e) in
          loop modul
    end in
  loop (Module.make (Module.Name.from_string "__repl__"))
