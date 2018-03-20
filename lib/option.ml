let is_some function
  | None -> false
  | _ -> true

let is_none = (not is_some)

let map fn = function
  | None -> None
  | Some x -> Some (fn x)

let flat_map fn = function
  | None -> None
  | Some x -> fn x

let get_else opt v = match opt with
  | None -> v
  | Some x -> x