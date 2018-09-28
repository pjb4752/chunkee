type t =
  | StrType of string
  | FnType of t list

val from_string: string -> t

val from_list: t list -> t

val to_string: t -> string
