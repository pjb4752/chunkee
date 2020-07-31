type t

val from_name: Mod_name.t -> t

val from_parts: Mod_name.Path.t -> Mod_name.Segment.t -> t

val name: t -> Mod_name.t

val short_name: t -> Mod_name.Segment.t

val path_list: t -> Mod_name.Segment.t list

val find_var: t -> Identifier.t -> Var.t option

val var_exists: t -> Identifier.t -> bool

val find_type: t -> Identifier.t -> Type.t option

val type_exists: t -> Identifier.t -> bool

(* TODO use Var here? *)
val define_var: t -> Identifier.t -> Type.t -> t

(*TODO use VarDef here instead of tuple*)
val define_record: t -> Identifier.t -> Type.rec_cons_t -> t

val to_string: t -> string
