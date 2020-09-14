type t

val with_name: Module_name.t -> t

val with_path_and_base: Module_name.Path.t -> Module_name.Segment.t -> t

val name: t -> Module_name.t

val basename: t -> Module_name.Segment.t

val path_segments: t -> Module_name.Segment.t list

val find_var: t -> Identifier.t -> Var.t option

val var_exists: t -> Identifier.t -> bool

val find_type: t -> Identifier.t -> Type.t option

val type_exists: t -> Identifier.t -> bool

(* TODO use Var here? *)
val define_var: t -> Identifier.t -> Type.t -> t

val inspect: t -> string
