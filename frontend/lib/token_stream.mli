type stream_t = Positional_stream.t
type t

val create: ?input_callback: (int -> stream_t option) -> stream_t -> t

val next: ?inside_form: bool -> t-> Token.t
