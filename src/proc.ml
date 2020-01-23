open Core


type 'a t =
  | P of (   input:UnixLabels.file_descr
          -> output:UnixLabels.file_descr
          -> error:UnixLabels.file_descr
          -> 'a)

let create_sys_process
      ~program
      ~args =
  let prog = program in
  let args = Array.of_list (List.cons program args) in
  P begin fun ~input ~output ~error ->
    let pid =
      UnixLabels.create_process ~prog ~args
        ~stdin:input
        ~stdout:output
        ~stderr:error
    in
    UnixLabels.waitpid ~mode:[] pid
    end


let run_process (P p)
      ?(input=UnixLabels.stdin)
      ?(output=UnixLabels.stdout)
      ?(error=UnixLabels.stderr)
      () =
  p ~input ~output ~error


let run_process' p =
  ignore(run_process p ())


let with_pipe f =
  let (r, w) = UnixLabels.pipe () in
  f ~read:r ~write:w


let pipe (P p1) (P p2) =
  with_pipe begin fun ~read ~write ->
    P begin fun ~input ~output ~error ->
        let a = Fun.protect
                  (fun _ -> p1 ~input ~output:write ~error)
                  ~finally:(fun _ -> UnixLabels.close write)
        in
        let b = Fun.protect
                  (fun _ -> p2 ~input:read ~output ~error)
                  ~finally:(fun _ -> UnixLabels.close read)
        in
        a, b
      end
  end


let ( ||> ) p1 p2 =
  P begin fun ~input ~output ~error ->
      let (_, r) = run_process (pipe p1 p2) ~input ~output ~error () in
      r
    end


let ( <|| ) p2 p1 = p1 ||> p2


let ( <<< ) (P p) str =
  with_pipe begin fun ~read ~write ->
    P begin fun ~input:_ ~output ~error ->
      let _ =
        Fun.protect
          (fun _ -> Unix.single_write ~buf:(Bytes.of_string str) write)
          ~finally:(fun _ -> Unix.close write) in
      Fun.protect
        (fun _ -> p ~input:read ~output ~error)
        ~finally:(fun _ -> Unix.close read)
      end
  end
