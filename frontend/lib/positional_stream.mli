type t

val of_string: ?line_number: int -> string -> t

val peek: t -> (Stream_position.t * char option)

val next: t -> (Stream_position.t * char option)

val next_only: t -> char

val junk: t -> unit
