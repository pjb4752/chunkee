type ('a, 'b) t = ('a, 'b) result

val is_ok: ('a, 'b) t -> bool

val is_error: ('a, 'b) t -> bool

val return: 'a -> ('a, _) t

val (>>=): ('a, 'b) t -> ('a -> ('c, 'b) t) -> ('c, 'b) t
