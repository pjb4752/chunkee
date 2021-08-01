open Printf

module List = struct
  include List

  let is_empty = function
    | [] -> true
    | _ -> false

  let filter_not f l =
    List.filter (fun x -> not (f x)) l

  let hd_else l e =
    match l with
    | [] -> e
    | hd :: _ -> hd

  let rec as_pairs = function
    | [] -> []
    | f :: n :: tl -> (f, n) :: as_pairs tl
    | _ -> assert false

  let zip left right =
    List.fold_left2 (fun acc l r -> (l, r) :: acc) [] left right |> List.rev

  let bind_left input_fn input_list =
    let fold_fn output_list element =
      Result.bind output_list (fun output_list ->
        Result.bind (input_fn element) (fun element ->
          Result.ok (element :: output_list)
        )
      )
    in
    List.fold_left fold_fn (Ok []) input_list

  let bind_right input_fn input_list =
    let fold_fn element output_list =
      Result.bind output_list (fun output_list ->
        Result.bind (input_fn element) (fun element ->
          Result.ok (element :: output_list)
        )
      )
    in
    List.fold_right fold_fn input_list (Ok [])

  let inspect fn l =
    sprintf "[%s]" @@ String.concat "; " @@ List.map fn l
end

module Map = struct
  module type S = sig
    include Map.S

    val find_else: key -> 'a -> 'a t -> 'a
  end

  module Make = functor (Ord: Map.OrderedType) -> struct
    module Map = Map.Make(Ord)
    include Map

    let find_else k v map =
      match Map.find_opt k map with
      | None -> v
      | Some v -> v
  end
end

module Option = struct
  include Option

  let return = Option.some

  let (>>=) = Option.bind

  let inspect o some_fn =
    match o with
    | Some v -> sprintf "Some (%s)" @@ some_fn v
    | None -> "None"

  module Syntax = struct
    let (let+) = Option.map

    let (let*) = Option.bind
  end
end

module Result = struct
  include Result

  let return = Result.ok

  let (>>=) = Result.bind

  let inspect ok_fn err_fn r =
    match r with
    | Ok v -> sprintf "Ok (%s)" @@ ok_fn v
    | Error e -> sprintf "Error (%s)" @@ err_fn e

  module Syntax = struct
    let (let+) = Result.map

    let (let*) = Result.bind
  end
end

module String = struct
  include String

let is_empty = String.equal ""

let is_blank s = is_empty @@ String.trim s

let sub_from s i =
  String.sub s i @@ String.length s - i

let to_chars s =
  let rec to_chars' i chars =
    if i < 0 then chars
    else to_chars' (i - 1) (s.[i] :: chars) in
  to_chars' (String.length s - 1) []

let append_char str chr = str ^ String.make 1 chr

let from_chars chars =
  List.fold_left append_char "" chars
end

