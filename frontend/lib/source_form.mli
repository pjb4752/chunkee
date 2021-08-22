type t = {
  position: Stream_position.t;
  value: u
} and u =
  | Number of float
  | String of string
  | Symbol of string
  | List of t list
  | Vector of t list

val create_number: Stream_position.t -> float -> t

val create_string: Stream_position.t -> string -> t

val create_symbol: Stream_position.t -> string -> t

val create_list: Stream_position.t -> t list -> t

val inspect: t -> string
