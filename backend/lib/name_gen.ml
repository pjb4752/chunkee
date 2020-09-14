open Printf

type t = int

let generator = 0

let next_value g =
  let g = g + 1 in (g, sprintf "__var%d" g)
