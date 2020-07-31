type t

type err_t =
  | ModuleError of string
  | NameError of string

type exists_in_scope = string -> bool

type exists_in_decls = Identifier.t -> Type.t option

val make: Pervasive.t -> Module.t -> t

val current_module: t -> Module.t

val resolve_name: t -> exists_in_scope -> Name_expr.t -> (Name.Var.t, err_t) result

val resolve_type: t -> ?lookup_fn:exists_in_decls option -> Type_expr.t -> (Type.t, err_t) result

val module_var: t -> Module_name.t -> Identifier.t -> Var.t option

val module_vartype: t -> Module_name.t -> Identifier.t -> Type.t option

val module_type: t -> Module_name.t -> Identifier.t -> Type.t option

val define_var: t -> Identifier.t -> Type.t -> t

val define_record: t -> Identifier.t -> Type.rec_cons_t -> t

val to_string: t -> string

val err_string: err_t -> string
