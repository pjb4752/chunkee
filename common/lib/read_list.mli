type elt = { value: char; line_num: int; char_num: int }

type t = elt list

val from_string: string -> t

val elt_debug_string: elt -> string

val debug_string: t -> string
