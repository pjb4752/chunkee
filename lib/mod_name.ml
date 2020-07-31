open Printf
open Thwack.Extensions

module Segment = struct
  type t = string

  let from_string s = s

  let compare l r = String.compare l r

  let to_string s = s

  let inspect s = sprintf "Segment(%s)" s
end

module Path = struct
  type t = Segment.t list

  let from_list = function
    | [] -> assert false
    | parts -> parts

  let to_list path = path

  let to_string path =
    let segments = List.map Segment.to_string path in
    String.concat "." segments

  let inspect path =
    sprintf "[%s]" @@ String.concat "; " @@ List.map Segment.inspect path
end

type t = Path.t * Segment.t

let make path segment = (path, segment)

let short_name (_, segment) = segment

let path_list (path, _) = Path.to_list path

let to_string (path, segment) =
  sprintf "%s.%s" (Path.to_string path) (Segment.to_string segment)

let to_list (path, segment) =
  List.append (Path.to_list path) [segment]

let inspect (path, segment) =
  sprintf "Mod_name(%s, %s)" (Path.inspect path) (Segment.inspect segment)
