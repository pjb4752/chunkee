type t

type exists_in_scope = string -> bool

type exists_in_decls = Type.Name.t -> Type.t option

val make: Pervasive.t -> Module.t -> t

val current_module: t -> Module.t

val resolve_name: t -> exists_in_scope -> Name_expr.t ->
  (Name.Var.t, Cmpl_err.t) result

val resolve_type: t -> ?lookup_fn:exists_in_decls option -> Type_expr.t ->
  (Type.t, Cmpl_err.t) result

val module_type: t -> Mod_name.t -> Var.Name.t -> Type.t option

val define_record: t -> Type.Name.t -> (Type.Name.t * Type.t) list -> t

val to_string: t -> string
