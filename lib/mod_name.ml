open Printf
open Thwack.Extensions

module Name = Id

module Path = struct
  type t = Name.t list

  let from_list = function
    | [] -> assert false
    | parts -> parts

  let to_list path = path

  let to_string path =
    let names = List.map Name.to_string path in
    String.concat "." names

  let inspect path =
    sprintf "[%s]" @@ String.concat "; " @@ List.map Name.to_string path
end

type t = Path.t * Name.t

let make path name = (path, name)

let short_name (_, name) = name

let path_list (path, _) = Path.to_list path

let to_string (path, name) =
  sprintf "%s.%s" (Path.to_string path) (Name.to_string name)

let to_list (path, name) =
  List.append (Path.to_list path) [name]

let inspect (path, name) =
  sprintf "Mod_name(%s, %s)" (Path.inspect path) (Name.to_string name)
