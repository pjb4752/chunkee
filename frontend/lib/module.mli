type t

val with_name: Module_name.t -> t

val with_path_and_base: Module_name.Path.t -> Module_name.Segment.t -> t

val name: t -> Module_name.t

val basename: t -> Module_name.Segment.t

val path_segments: t -> Module_name.Segment.t list

val find_variable: t -> string -> Var.t option

val variable_exists: t -> string -> bool

val find_type: t -> string -> Type.t option

val type_exists: t -> string -> bool

(* TODO use Var here? *)
val define_variable: t -> string -> Type.t -> t

val inspect: t -> string
