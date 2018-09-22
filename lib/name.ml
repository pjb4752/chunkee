open Printf

module V = Var
module T = Type

module Var = struct
  type t =
    | Local of string
    | Module of Mod_name.t * V.Name.t

  let to_string = function
    | Local s -> sprintf "(local %s)" s
    | Module (qn, vn) -> sprintf "(module %s/%s)"
      (Mod_name.to_string qn) (V.Name.to_string vn)
end

module Type = struct
  type t =
    | Builtin of string
    | UserDef of Mod_name.t * T.Name.t

  let to_string = function
    | Builtin s -> sprintf "%s" s
    | UserDef (qn, vn) -> sprintf "%s/%s"
      (Mod_name.to_string qn) (T.Name.to_string vn)
end
