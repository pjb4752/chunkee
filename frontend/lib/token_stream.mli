type stream_t = Positional_stream.t
type t

exception Error of (Stream_position.t * string)

val create: ?input_callback: (int -> stream_t option) -> stream_t -> t

val next: ?inside_form: bool -> t-> Token.t
