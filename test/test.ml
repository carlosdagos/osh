let () =
  let open Osh.Proc in
  run_process' begin
        [%osh "echo", ["hello world"]]
    ||> [%osh "wc", ["-l"]]
  end
