type 'a t = 'a option

let is_some = function
  | None -> false
  | _ -> true

let is_none opt = not (is_some opt)

let return v = Some v

let (>>=) opt fn =
  match opt with
  | None -> None
  | Some v -> fn v

let get_else opt v =
  match opt with
  | None -> v
  | Some x -> x
