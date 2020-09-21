open Printf

let verbose = ref false
let main = ref None

let set_main filename = main := Some filename

let read_source_file filename =
  let in_channel = open_in filename in
  try
    let file_length = in_channel_length in_channel in
    let source = really_input_string in_channel file_length in
    let () = close_in in_channel in
    source
  with e ->
    let () = close_in_noerr in_channel in
    raise e

let create_module path_segments name =
  let path_segments = List.map Frontend.Module_name.Segment.from_string path_segments in
  let module_path = Frontend.Module_name.Path.from_segments path_segments in
  let module_name = Frontend.Module_name.Segment.from_string name in
  Frontend.Module.with_path_and_base module_path module_name

let compile_module table source =
  Frontend.Compile.compile_module table source

let compile filename =
  let pervasive = Backend.Stdlib.pervasive in
  let modul = create_module ["test"] "main" in
  let table = Frontend.Symbol_table.make pervasive modul in
  let source = read_source_file filename in
  match compile_module table source with
  | Error e -> Frontend.Cmpl_err.to_string e
  | Ok (_, nodes) ->
      let lua = Backend.Emit.emit_node_strings nodes in
      String.concat "\n\n" lua

let main () =
  let usage = "Usage: kludge [options] [main]" in
  let () = Arg.parse [
    ("-v", Set verbose, "display output from compilation stages")
  ] set_main usage in
  match !main with
  | None -> printf "no main given!\n"
  | Some filename -> begin
    let source = compile filename in
    printf "%s\n" source
  end

let () = main ()
