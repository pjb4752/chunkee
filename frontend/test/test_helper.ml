open Chunkee
open Chunkee.Ast

module PNode = Parsed_node
module RNode = Resolved_node

let make_bare_sym name =
  PNode.SymLit (Name_expr.BareName name)

let make_qual_sym mod_name name =
  let mod_parts = String.split_on_char '.' mod_name in
  let mod_parts = List.map (Mod_name.Name.from_string) mod_parts in
  match List.rev mod_parts with
  | mod_name :: path_parts -> begin
    let mod_path = Mod_name.Path.from_list path_parts in
    let mod_name = Mod_name.make mod_path mod_name in
    PNode.SymLit (Name_expr.QualName (mod_name, name))
  end
  | _ -> assert false

let make_local_sym name =
  RNode.SymLit (Name.Var.Local name)

let make_mod_sym mod_name sym_name =
  RNode.SymLit (Name.Var.Module (mod_name, sym_name))

let make_type_expr type_name =
  let type_name = Name_expr.BareName type_name in
  Type_expr.SimpleType type_name

let make_p_var_def var_name type_name =
  let var_name = PNode.VarDef.Name.from_string var_name in
  let var_type = make_type_expr type_name in
  PNode.VarDef.from_parts var_name var_type

let make_r_var_def var_name tipe =
  let var_name = RNode.VarDef.Name.from_string var_name in
  RNode.VarDef.from_parts var_name tipe

let make_p_binding name expr =
  let name = PNode.Binding.Name.from_string name in
  PNode.Binding.from_node name expr

let make_r_binding name expr =
  let name = Resolved_node.Binding.Name.from_string name in
  Resolved_node.Binding.from_node name expr

