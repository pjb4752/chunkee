type t = {
  char_stream: char Stream.t;
  position: Stream_position.t ref;
}

let of_string ?line_number:(line_number=1) s = {
  char_stream = Stream.of_string s;
  position = ref {
    Stream_position.line_number;
    char_number = 0;
  }
}

let peek positional_stream =
  let char_stream = positional_stream.char_stream in
  let position = !(positional_stream.position) in
  match Stream.peek char_stream with
  | None -> (position, None)
  | Some c -> begin
    let new_char_number = position.char_number + 1 in
    let new_position = { position with char_number = new_char_number } in
    (new_position, Some c)
  end

let next positional_stream =
  let char_stream = positional_stream.char_stream in
  match peek positional_stream with
  | (position, None) -> (position, None)
  | (position, Some c) -> begin
    let () = positional_stream.position := position in
    let () = Stream.junk char_stream in
    (position, Some c)
  end

let next_only positional_stream =
  match next positional_stream with
  | (_, None) -> raise Stream.Failure
  | (_, Some c) -> c

let junk positional_stream =
  let _ = next positional_stream in ()
