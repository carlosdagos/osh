open Core

module Streams = struct
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
      let is_newline c = Char.equal c '\n' in
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
end

module System = struct
  exception ExecutableNotFound of string

  let executable_paths () =
    let open Sys in
    String.split_on_chars (getenv_exn "PATH") ~on:[':']
    |> begin fun ps ->
       List.filter ps ~f:begin fun path ->
         match is_directory ~follow_symlinks:true path with
         | `Yes -> true
         | _    -> false
         end
       end


  let find_all_executables exe =
    let open Sys in
    let searchpaths =
      List.map (executable_paths ()) ~f:begin fun sp ->
        sp ^ "/" ^ exe
        end
    in
    List.filter searchpaths ~f:begin fun sp ->
      match file_exists ~follow_symlinks:true sp with
      | `Yes -> true
      | _    -> false
      end


  let find_executable exe =
    match List.hd (find_all_executables exe) with
    | Some p -> p
    | None -> raise (ExecutableNotFound exe)
end
