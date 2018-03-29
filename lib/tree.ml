open Extensions
open Option
open Printf

module Children = Map.Make(Module.Name)

type t =
  | Node of t Children.t
  | Leaf of Module.t

let make root = Node Children.empty

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
  find' (Some tree) paths

let rec insert_module tree paths modul =
  match tree with
  | Leaf _ -> assert false
  | Node children ->
      match paths with
      | [] ->
          let name = Module.name modul in
          if Children.mem name children then assert false
          else Leaf modul
      | p :: ps ->
          let child_tree = Children.find_else p (make ()) children in
          let new_children = insert_module child_tree ps modul in
          Node (Children.add p new_children children)

let rec to_string tree =
  let fold_fn k v prior =
    let name = Module.Name.to_string k in
    sprintf "(%s %s)" name (to_string v) :: prior in
  let string_of_children children = Children.fold fold_fn children [] in
  match tree with
  | Node cs -> sprintf "(node %s)" (String.concat " " (string_of_children cs))
  | Leaf modul -> sprintf "(leaf %s)" (Module.to_string modul)

(*module Zipper = struct*)
  (*type tree_t = t*)
  (*type t = tree_t * tree_t list*)

  (*let from_tree tree = (tree, [])*)

  (*let rec to_tree zipper =*)
    (*match zipper with*)
    (*| (node, []) -> node*)
    (*| (subtree, p :: ps) -> begin*)
        (*match p with*)
        (*| Node (nk, parent) ->*)
            (*let sk = key subtree in*)
            (*let new_modules = ModuleSet.add sk subtree parent in*)
            (*to_tree (Node (nk, new_modules), ps)*)
        (*| _ -> assert false*)
    (*end*)

  (*let rec find_child zipper = function*)
    (*| [] -> Some zipper*)
    (*| (p :: ps) -> begin*)
      (*match zipper with*)
      (*| (Leaf _, parents) -> None*)
      (*| (Node (n, node), parents) -> begin*)
        (*match ModuleSet.find_opt p node with*)
        (*| None -> None*)
        (*| Some subtree ->*)
            (*let new_parents = Node (n, node) :: (snd zipper) in*)
            (*find_child (subtree, new_parents) ps*)
      (*end*)
    (*end*)

  (*let find_module zipper path =*)
    (*match find_child zipper path with*)
    (*| Some (Leaf (_, modul), _) -> Some modul*)
    (*| _ -> None*)

  (*let insert_child zipper name child =*)
    (*match zipper with*)
    (*| (Leaf _, _) -> None*)
    (*| (Node (k, node), parents) ->*)
        (*if ModuleSet.mem name node then None*)
        (*else Some (Node (k, ModuleSet.add name child node), parents)*)

(*end*)
