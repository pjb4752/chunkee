open Printf

type atom_t =
  | Number of float
  | String of string
  | Symbol of string

type marker_t =
  | LeftParen
  | RightParen

type token_t =
  | EndOfInput
  | Atom of atom_t
  | Marker of marker_t

type t = {
  position: Stream_position.t;
  value: token_t
}

let inspect_atom = function
  | Number n -> sprintf "Token.Number(%f)" n
  | String s -> sprintf "Token.String(\"%s\")" s
  | Symbol s -> sprintf "Token.Symbol(%s)" s

let inspect_marker = function
  | LeftParen -> "Token.LeftParen"
  | RightParen -> "Token.RightParen"

let inspect { position; value } =
  let position = Stream_position.inspect position in
  let value = match value with
  | EndOfInput -> "Token.EndOfInput"
  | Atom atom -> inspect_atom atom
  | Marker marker -> inspect_marker marker
  in
  sprintf "{ position = %s; value = %s }" position value
