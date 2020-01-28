[%%osh_def ["cat"; "awk"; "grep"; "echo"; "ip"]]


let%osh_script my_script =
      echo ["hello world"]
  ||> grep ["hello"]
  ||> awk  ["{print $1}"]


let%osh_script hello_world_script ?(message="hello_world") () =
  let message = message ^ "trololol" in
  let echo = echo [message] in
  echo


let%osh_script my_ip =
      ip ["route"; "get"; "8.8.8.8"]
  ||> awk ["/src / {print $7}"]


let%expect_test _ =
  let open Osh.Proc in
  let message = "hello world"
  in
  let inline_script = [%osh "echo"] [message] ||> [%osh "cat"] []
  in

  run_process' inline_script;
  run_process' (hello_world_script ());
  run_process' my_script;
  run_process' my_ip;

  [%expect{|
  hello world
  hello_world
  hello
  192.168.95.129
  |}]
