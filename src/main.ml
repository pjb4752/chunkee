let print_list string_fn l =
  List.iter (fun i -> string_fn i |> (Printf.printf "%s\n")) l

let print_forms = print_list Form.to_string
let print_nodes nodes =
  let print_fn = function
    | Ok n -> Node.to_string n
    | Error s -> Analyze.CompileError.to_string s in
  print_list print_fn nodes

let eval repl_mod line = Read.read line |> (Analyze.analyze repl_mod)

let main () =
  let repl_mod = Module.make (Module.Name.from_string "__repl__") in
  let rec loop () =
    let line = read_line () in
    if line = "(quit)" then Printf.printf "Goodbye\n"
    else eval repl_mod line |> print_nodes |> loop in
  loop ()
