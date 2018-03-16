let map fn = function
  | Ok v -> Ok (fn v)
  | Error e -> Error e

