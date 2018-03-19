let is_ok = function
  | Ok _ -> true
  | Error _ -> false

let is_error r = not (is_ok r)

let map fn = function
  | Ok v -> Ok (fn v)
  | Error e -> Error e

let flat_map fn = function
  | Ok v -> fn v
  | Error e -> Error e
