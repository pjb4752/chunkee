open Names

type t

type err_t =
  | ModuleError of string
  | NameError of string

type exists_in_scope = string -> bool

type exists_in_decls = Identifier.t -> Type.t option

val make: Intrinsics.t -> Module.t -> t

val current_module: t -> Module.t

val resolve_name: t -> exists_in_scope -> Unresolved_name.t -> (Resolved_name.t, err_t) result

val resolve_type: t -> ?lookup_fn:exists_in_decls option -> Type_expr.t -> (Type.t, err_t) result

val find_variable: t -> Module_name.t -> Identifier.t -> Var.t option

val find_variable_type: t -> Module_name.t -> Identifier.t -> Type.t option

val find_type: t -> Module_name.t -> Identifier.t -> Type.t option

val define_variable: t -> Identifier.t -> Type.t -> t

val inspect: t -> string

val err_string: err_t -> string
