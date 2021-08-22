open Printf
open Common.Extensions
open Common.Extensions.Result

module Result = struct
  type t = (Source_form.t list, Compile_error.t) result

  let inspect result =
    let result = (Result.inspect (List.inspect Source_form.inspect) Compile_error.to_string) result in
    sprintf "Parsing.Result(%s)" result
end

let raise_error position message =
  raise (Token.Error (position, message))

let parse_atom position atom =
  let atom = match atom with
  | Token.Number n -> Source_form.Number n
  | Token.String s -> Source_form.String s
  | Token.Symbol s -> Source_form.Symbol s
  in { Source_form.position; value = atom }

let parse_list token_stream starting_position =
  let rec parse_list' elements starting_position =
    let { Token.position; value } = Token_stream.next token_stream in
    match value with
    | Token.EndOfInput -> raise_error position "unexpected end of input"
    | Token.Marker token -> begin
      match token with
      | RightParen -> {
        Source_form.position = starting_position;
        value = Source_form.List (List.rev elements)
      }
      | LeftParen -> begin
        let parsed_form = parse_list' [] position in
        parse_list' (parsed_form :: elements) starting_position
      end
    end
    | Token.Atom token -> begin
      let parsed_form = parse_atom position token in
      parse_list' (parsed_form :: elements) starting_position
    end
  in
  parse_list' [] starting_position

let parse_incremental token_stream =
  let rec loop parsed_forms =
    let { Token.position; value } = Token_stream.next token_stream ~inside_form:false in
    match value with
    | Token.EndOfInput -> List.rev parsed_forms
    | Token.Marker token -> begin
      let parsed_form = match token with
      | RightParen -> raise_error position "unmatched delimiter ')'"
      | LeftParen -> parse_list token_stream position
      in loop (parsed_form :: parsed_forms)
    end
    | Token.Atom token -> begin
        let parsed_form = parse_atom position token in
        loop (parsed_form :: parsed_forms)
    end
  in
  try Ok (loop [])
  with Token.Error (position, message) ->
    Error (Compile_error.create_syntax_error position message)
