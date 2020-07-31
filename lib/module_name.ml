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

  let from_segments = function
    | [] -> assert false
    | segments -> segments

  let to_segments segments = segments

  let to_string segments =
    let segments = List.map Segment.to_string segments in
    String.concat "." segments

  let inspect segments =
    sprintf "[%s]" @@ String.concat "; " @@ List.map Segment.inspect segments
end

type t = Path.t * Segment.t

let from_path_and_base path base = (path, base)

let base (_, b) = b

let path (p, _) = p

let to_segments (path, base) =
  List.append (Path.to_segments path) [base]

let path_segments (path, _) = Path.to_segments path

let to_string (path, base) =
  sprintf "%s.%s" (Path.to_string path) (Segment.to_string base)

let inspect (path, base) =
  sprintf "Mod_name(%s, %s)" (Path.inspect path) (Segment.inspect base)
