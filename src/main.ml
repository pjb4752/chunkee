let print forms =
  let print_all = List.iter (Printf.printf "%s\n") in
    List.map Form.to_string forms |> print_all

let eval = Read.read

let main () =
  let rec loop () =
    let line = read_line () in
      if line = "(quit)" then Printf.printf "Goodbye\n"
      else eval line |> print |> loop in
    loop ()
