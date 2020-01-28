(* This module tests that we can create inline scripts
   which are of type Osh.Proc.t *)

[%%osh_def ["cat"; "awk"; "grep"; "echo"]]


let%osh_script my_script =
      echo ["Hello World!"]
  ||> grep ["hello"; "-i"]
  ||> awk  ["{print $1}"]


let%osh_script hello_world_script ?(message="hello_world") () =
  let message = message ^ " appended" in
  let echo = echo [message] in
  echo

let%expect_test _ =
  let open Osh.Proc in
  let message = "Hello Osh!" in

  run_process' my_script;
  run_process' (hello_world_script ~message ());
  [%expect{|
  Hello
  Hello Osh! appended
  |}]
