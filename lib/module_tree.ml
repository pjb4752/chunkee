open Thwack.Extensions
open Thwack.Extensions.Option
open Thwack.Extensions.Option.Syntax
open Printf

module Lib_tree = struct
  module Children = Map.Make(Module_name.Segment)

  type t =
    | Node of t Children.t
    | Leaf of Module.t

  let empty = Node Children.empty

  let leaf_exists children name =
    match Children.find_opt name children with
    | None -> false
    | Some (Node _) -> false
    | Some (Leaf _) -> true

  let find_module tree paths =
    let rec find' tree paths =
      let match_fn tree paths =
        match tree with
        | Leaf modul when List.is_empty paths -> Some modul
        | Node node when not (List.is_empty paths) ->
          let head = List.hd paths in
          find' (Children.find_opt head node) (List.tl paths)
        | _ -> None in
      let* tree = tree in
      let* modul = match_fn tree paths in
      return modul in
    find' (Some tree) (Module_name.to_segments paths)

  let insert_module tree modul =
    let rec insert' tree paths modul =
      match tree with
      | Leaf _ -> assert false
      | Node children ->
          match paths with
          | [] ->
              let name = Module.basename modul in
              if Children.mem name children then assert false
              else Children.add name (Leaf modul) children
          | p :: ps ->
              let child_tree = Children.find_else p empty children in
              let new_node = Node (insert' child_tree ps modul) in
              Children.add p new_node children in
    Node (insert' tree (Module.path_segments modul) modul)

  let update_module tree modul =
    let rec update' tree paths modul =
      match tree with
      | Leaf _ -> assert false
      | Node children ->
          match paths with
          | [] ->
              let name = Module.basename modul in
              if leaf_exists children name then
                Children.add name (Leaf modul) children
              else assert false
          | p :: ps -> begin
            match Children.find_opt p children with
            | None -> assert false
            | Some child_tree -> begin
              let new_node = Node (update' child_tree ps modul) in
              Children.add p new_node children
            end
          end in
    Node (update' tree (Module.path_segments modul) modul)

  let rec inspect tree =
    let fold_fn name_segment subtree prior =
      let name = Module_name.Segment.inspect name_segment in
      sprintf "Child(%s, %s)" name (inspect subtree) :: prior in
    let inspect_children children = Children.fold fold_fn children [] in
    match tree with
    | Node cs -> sprintf "Node(%s)" (String.concat " " (inspect_children cs))
    | Leaf modul -> sprintf "Leaf(%s)" (Module.inspect modul)
end

type t = Lib_tree.t

let empty = Lib_tree.empty

let find_module lib_tree name =
  Lib_tree.find_module lib_tree name

let insert_module lib_tree modul =
  Lib_tree.insert_module lib_tree modul

let update_module lib_tree modul =
  Lib_tree.update_module lib_tree modul

let inspect lib_tree =
  Lib_tree.inspect lib_tree
