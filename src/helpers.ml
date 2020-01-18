open Core

let stream_map stream f =
    let next _i =
      try Some (f (Stream.next stream))
      with Stream.Failure -> None in
    Stream.from next


let next_while stream f =
  let rec do_rec buf =
    let item = Stream.peek stream in
    match item with
      Some i -> if f i then
                  begin
                    buf := !buf@[Stream.next stream];
                    do_rec buf
                  end
                else
                  !buf
    | None -> !buf
  in
  let buf = ref [] in
  do_rec buf


let stream_chars_to_strings stream =
  let rec next i =
    let is_newline c = phys_equal c '\n' in
    let not_newline c = not (is_newline c) in
    let chars = next_while stream not_newline in
    try
      if not (List.is_empty chars)
      then
        (* We have some characters *)
        Some (String.of_char_list chars)
      else
        (* We got nothing, but it could be that there's
           a bunch of newlines at the beginning *)
        begin
          let newlines = next_while stream is_newline in
          if List.is_empty newlines
          then
            (* Then we must be at the end of the stream...? *)
            None
          else
            (* Well we've just consumed the stream full of newlines
               so start over the whole logic *)
            next i
        end
    with
      Stream.Failure -> None
  in
  Stream.from next
