open Printf

let parent_prompt = "newchonk> "
let nested_prompt = "       |> "

let display text =
  let () = output_string stdout text in flush stdout

let read_stream ?line_number:(line_number=1) message =
  let input = display message |> read_line in
  Frontend.Positional_stream.of_string ~source:"stdin" ~line_number:line_number input

let evaluate_forms symbol_table source_forms =
  let evaluate_in_stages symbol_table source_form =
    let open Common.Extensions.Result.Syntax in
    let* source_form = source_form in
    let* semantic_form = Frontend.Semantic_parsing.parse_form source_form in
    let* symbol_table = Frontend.Identifier_definition.define_identifiers symbol_table semantic_form in
    let* resolved_form = Frontend.Identifier_resolution.resolve_identifiers symbol_table semantic_form in
    Result.ok (symbol_table, resolved_form)
  in
  let evaluate_form symbol_table source_form =
    match evaluate_in_stages symbol_table source_form with
    | Error compile_error ->
        let () = printf "%s\n" @@ Frontend.Compile_error.to_string compile_error in
        symbol_table
    | Ok (symbol_table, resolved_form) ->
        let () = printf "%s\n" @@ Frontend.Ast.Resolved_form.inspect resolved_form in
        symbol_table
  in
  List.fold_left evaluate_form symbol_table source_forms

let await_input previous_line =
  let line_number = previous_line + 1 in
  Some (read_stream ~line_number:line_number nested_prompt)

let rec loop symbol_table =
  let stream = read_stream parent_prompt in
  let token_stream = Frontend.Token_stream.create ~input_callback:await_input stream in
  Frontend.Source_parsing.parse_incremental token_stream |> evaluate_forms symbol_table |> loop

let run symbol_table =
  try loop symbol_table
  with End_of_file -> display "bye!\n"
