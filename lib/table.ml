open Extensions
open Option
open Printf

module Children = Map.Make(Module.Name)

type t =
  | Node of t Children.t
  | Leaf of Module.t

let empty = Node Children.empty

let find_module tree paths =
  let rec find' tree paths =
    let match_fn tree paths =
      match tree with
      | Leaf modul when List.is_empty paths -> Some modul
      | Node node when not (List.is_empty paths) ->
        let head = List.hd paths in
        find' (Children.find_opt head node) (List.tl paths)
      | _ -> None in
    tree >>= fun t ->
    (match_fn t paths) >>= fun m ->
    return m in
  find' (Some tree) (Module.Path.to_list paths)

let insert_module tree modul =
  let rec insert' tree paths modul =
    match tree with
    | Leaf _ -> assert false
    | Node children ->
        match paths with
        | [] ->
            let name = Module.name modul in
            if Children.mem name children then assert false
            else Children.add name (Leaf modul) children
        | p :: ps ->
            let child_tree = Children.find_else p empty children in
            let new_node = Node (insert' child_tree ps modul) in
            Children.add p new_node children in
  Node (insert' tree (Module.path_list modul) modul)

let leaf_exists children name =
  match Children.find_opt name children with
  | None -> false
  | Some (Node _) -> false
  | Some (Leaf _) -> true

let update_module tree modul =
  let rec update' tree paths modul =
    match tree with
    | Leaf _ -> assert false
    | Node children ->
        match paths with
        | [] ->
            let name = Module.name modul in
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
  Node (update' tree (Module.path_list modul) modul)

let with_stdlib =
  List.fold_left (fun t m -> insert_module t m) empty Stdlib.core

let rec to_string tree =
  let fold_fn k v prior =
    let name = Module.Name.to_string k in
    sprintf "(%s %s)" name (to_string v) :: prior in
  let string_of_children children = Children.fold fold_fn children [] in
  match tree with
  | Node cs -> sprintf "(node %s)" (String.concat " " (string_of_children cs))
  | Leaf modul -> sprintf "(leaf %s)" (Module.to_string modul)
