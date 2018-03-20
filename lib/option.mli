type 'a t = 'a option

val is_some: 'a t -> bool

val is_none: 'a t -> bool

val return: 'a -> 'a t

val (>>=): 'a t -> ('a -> 'b t) -> 'b t

val get_else: 'a t -> 'a -> 'a
