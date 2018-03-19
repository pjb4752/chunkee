val is_ok: ('a, 'b) result -> bool

val is_error: ('a, 'b) result -> bool

val map: ('a -> 'b) -> ('a, 'c) result -> ('b, 'c) result

val flat_map: ('a -> ('b, 'c) result) -> ('a, 'c) result -> ('b, 'c) result
