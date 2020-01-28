(* This module tests that we can create inline scripts in
   an expression *)

let%expect_test _ =
  let open Osh.Proc in
  let message = "Hello Osh!" in
  let echo = [%osh "echo"] in
  let cat  = [%osh "cat"] in
  let inline_script =  echo [message] ||> cat []
  in
  run_process' inline_script;
  [%expect{|
  Hello Osh!
  |}]
