(* This module tests that we can define some programs
   as structure items *)

[%%osh_def ["cat"; "echo"]]

let%expect_test _ =
  let open Osh.Proc in
  let message = "Hello Osh!" in
  let inline_script =  echo [message] ||> cat []
  in
  run_process' inline_script;
  [%expect{|
  Hello Osh!
  |}]
