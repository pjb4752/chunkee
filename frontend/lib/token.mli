exception Error of (Stream_position.t * string)

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

val inspect: t -> string
