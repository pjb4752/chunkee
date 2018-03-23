open Printf

let print_list string_fn l =
  List.iter (fun i -> string_fn i |> (printf "%s\n")) l

let print_forms = print_list Form.to_string
let print_nodes nodes =
  let print_fn = function
    | Ok n -> Node.to_string n
    | Error s -> Analyze.CompileError.to_string s in
  print_list print_fn nodes
let print_module (modul, errors) =
  let () = printf "%s\n" (Module.to_string modul) in
  if (List.length errors = 0) then ()
  else
    let errors = List.map Resolve.NameError.to_string errors in
    printf "%s\n" (String.concat "\n" errors)

let print_debug forms nodes modul =
  let () = print_forms forms in
  let () = print_nodes nodes in
  print_module modul

let read line =
  try Ok (Read.read line)
  with Read.SyntaxError e -> Error e

let analyze repl_mod forms =
  let nodes = List.map Analyze.analyze forms in
  let fold_fn node (modul, errors) =
    match node with
    | Ok node -> begin
        match Resolve.define_vars modul node with
        | Ok modul -> (modul, errors)
        | Error e -> (modul, e :: errors)
    end
    | Error _ -> (modul, errors) in
  let modul = List.fold_right fold_fn nodes (repl_mod, []) in
  Ok (forms, nodes, modul)

let eval repl_mod line =
  match read line with
  | Ok forms -> analyze repl_mod forms
  | Error e -> Error e

let main () =
  let rec loop modul =
    let () = printf "-> " in
    let line = read_line () in
    if line = "(quit)" then printf "Goodbye\n"
    else begin
      match eval modul line with
      | Ok (forms, nodes, modul) ->
          let () = print_debug forms nodes modul in
          loop (fst modul)
      | Error e ->
          let () = printf "%s\n" e in
          loop modul
    end in
  loop (Module.make (Module.Name.from_string "__repl__"))
