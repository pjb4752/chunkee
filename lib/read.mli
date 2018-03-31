exception SyntaxError of string

val read_exn: string -> Form.t list

val read: string -> (Form.t list, Cmpl_err.t) result
