type t

val from_name: Mod_name.t -> t

val from_parts: Mod_name.Path.t -> Mod_name.Name.t -> t

val name: t -> Mod_name.t

val short_name: t -> Mod_name.Name.t

val path_list: t -> Mod_name.Name.t list

val find_var: t -> Var.Name.t -> Var.t option

val var_exists: t -> Var.Name.t -> bool

val find_type: t -> Type.Name.t -> Type.t option

val type_exists: t -> Type.Name.t -> bool

(* TODO use Var here? *)
val define_var: t -> Var.Name.t -> Type.t -> t

(*TODO use VarDef here instead of tuple*)
val define_record: t -> Type.Name.t -> Type.rec_cons_t -> t

val to_string: t -> string
