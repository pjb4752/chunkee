open Printf

let verbose = ref false
let interactive = ref false
let input_files = ref []

let set_input_files filename =
  input_files := filename :: !input_files

let read_next_line in_channel =
  try Some (input_line in_channel)
  with End_of_file -> None

let create_module path_segments name =
  let path_segments = List.map Frontend.Module_name.Segment.from_string path_segments in
  let module_path = Frontend.Module_name.Path.from_segments path_segments in
  let module_name = Frontend.Module_name.Segment.from_string name in
  Frontend.Module.with_path_and_base module_path module_name

let compile_forms symbol_table source_forms =
  let compile_in_stages symbol_table source_form =
    let open Common.Extensions.Result.Syntax in
    let* source_form = source_form in
    let* semantic_form = Frontend.Semantic_parsing.parse_form source_form in
    let* symbol_table = Frontend.Identifier_definition.define_identifiers symbol_table semantic_form in
    let* resolved_form = Frontend.Identifier_resolution.resolve_identifiers symbol_table semantic_form in
    Result.ok (symbol_table, resolved_form)
  in
  let compile_form symbol_table source_form =
    match compile_in_stages symbol_table source_form with
    | Error compile_error ->
        let () = printf "%s\n" @@ Frontend.Compile_error.to_string compile_error in
        symbol_table
    | Ok (symbol_table, resolved_form) ->
        let () = printf "%s\n" @@ Frontend.Ast.Resolved_form.inspect resolved_form in
        symbol_table
  in
  List.fold_left compile_form symbol_table source_forms

let next_input_line in_channel previous_line_number =
  let line_number = previous_line_number + 1 in
  match read_next_line in_channel with
  | None -> None
  | Some line -> Some (Frontend.Positional_stream.of_string ~source:"file" ~line_number:line_number line)

let compile_file symbol_table filename =
  let next_input_line = next_input_line @@ open_in filename in
  match next_input_line 0 with
  | None -> symbol_table
  | Some positional_stream ->
      let token_stream = Frontend.Token_stream.create ~input_callback:next_input_line positional_stream in
      Frontend.Source_parsing.parse_incremental token_stream |> compile_forms symbol_table

let compile_all filenames =
  let intrinsics = Backend.Stdlib.intrinsics in
  let tmp_module = create_module ["tmp"] "main" in
  let symbol_table = Frontend.Symbol_table.make intrinsics tmp_module in
  List.fold_left compile_file symbol_table filenames

let main () =
  let usage = "Usage: kludge [options] [input files]" in
  let () = Arg.parse [
    ("-v", Arg.Set verbose, "display output from compilation stages");
    ("-i", Arg.Set interactive, "enter interactive session");
  ] set_input_files usage in
  match !input_files with
  | [] when not !interactive -> printf "no input files given!\n"
  | files when not !interactive -> let _ = compile_all files in ()
  | files -> compile_all files |> Repl.run

let () = main ()
