type t = {
  position: Stream_position.t;
  value: u
} and u =
  | Number of float
  | String of string
  | Symbol of string
  | List of t list
  | Vector of t list

val inspect: t -> string
