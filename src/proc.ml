open Core


type t = Unix.Process_info.t * char Stream.t


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
  let searchdirs = executable_paths ()
  in
  List.filter searchdirs ~f:begin fun dir ->
    let exe_full_name = dir ^ "/" ^ exe in
    match file_exists ~follow_symlinks:true exe_full_name with
    | `Yes -> true
    | _    -> false
  end


let find_executable exe =
  match List.hd (find_all_executables exe) with
  | Some p -> p
  | None -> raise (ExecutableNotFound exe)


let create_process_stream ~prog ~args =
  let proc_info = Unix.create_process ~prog ~args in
  let stdout_chan = Unix.in_channel_of_descr proc_info.stdout in
  proc_info, Stream.of_channel stdout_chan


let () =
  let open Ppxlib in
  (* This is our expand function. For a given list of
     programs, it will create a `let` expression that
     calls our executable program. *)
  let expand_fn ~loc ~path programs =
    ignore (path, programs);
    [%expr 1 + 1]
  in
  let extension =
    Extension.declare
      "osh_progs"
      Extension.Context.expression
      Ast_pattern.(single_expr_payload (elist (estring __)))
      expand_fn in
  let rule =
    Context_free.Rule.extension extension
  in
  Driver.register_transformation ~rules:[rule] "osh_transformation"
