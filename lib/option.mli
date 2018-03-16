val is_some: 'a option -> bool

val is_none: 'a option -> bool

val map: ('a -> 'b) -> 'a option -> 'b option

val flat_map: ('a -> b' option) -> a' option -> b' option

val get_else: 'a option -> 'a -> 'a
