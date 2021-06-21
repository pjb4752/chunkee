open Printf
open Common.Extensions
open Common.Extensions.Result
open Common.Extensions.Result.Syntax

module Result = struct
  type value_t = (Source_form.t, Compile_error.t) result
  type t = value_t list

  let inspect values =
    let values = List.map (Result.inspect Source_form.inspect Compile_error.to_string) values in
    sprintf "Parsing.Result([%s])" @@ String.concat ", " values
end

let parse_atom position atom =
  let atom = match atom with
  | Token.Number n -> Source_form.Number n
  | Token.String s -> Source_form.String s
  | Token.Symbol s -> Source_form.Symbol s
  in { Source_form.position; value = atom }

let parse_list token_stream starting_position =
  let rec parse_list' elements =
    let* elements = elements in
    let { Token.position; value } = Token_stream.next token_stream in
    match value with
    | Token.EndOfInput -> Error (Compile_error.parse_errors position ["unexpected end of input"])
    | Token.Marker token -> begin
      match token with
      | RightParen -> return {
        Source_form.position = starting_position;
        value = Source_form.List (List.rev elements)
      }
      | LeftParen -> begin
        let* parsed_form = parse_list' (Ok []) in
        parse_list' (Ok (parsed_form :: elements))
      end
    end
    | Token.Atom token -> begin
      let parsed_form = parse_atom position token in
      parse_list' (Ok (parsed_form :: elements))
    end
  in
  parse_list' (Ok [])

let parse_incremental token_stream =
  let rec loop parsed_forms =
    let { Token.position; value } = Token_stream.next token_stream ~inside_form:false in
    match value with
    | Token.EndOfInput -> List.rev parsed_forms
    | Token.Marker token -> begin
      let parsed_form = match token with
      | RightParen -> Error (Compile_error.parse_errors position ["unexpected ')'"])
      | LeftParen -> parse_list token_stream position
      in loop (parsed_form :: parsed_forms)
    end
    | Token.Atom token -> begin
        let parsed_form = parse_atom position token in
        loop (Ok parsed_form :: parsed_forms)
    end
  in
  loop []
