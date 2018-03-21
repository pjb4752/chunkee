module List = struct
  include List

  let rec as_pairs = function
      | [] -> []
      | f :: n :: tl -> (f, n) :: as_pairs tl
      | _ -> assert false
end
