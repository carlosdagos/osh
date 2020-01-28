(** ['a t] is a base type that wraps a function that reads from [~input],
    writes to standard output to [~output], errors to [~error], and
    returns an ['a].
*)
type 'a t =
  | P of (   input:UnixLabels.file_descr
          -> output:UnixLabels.file_descr
          -> error:UnixLabels.file_descr
          -> 'a)


val identity : 'a t -> 'a t
(** [identity p] will return the same process provided. *)

val create_sys_process :
  program:string -> args:string list -> (int * Unix.process_status) t
(** [create_sys_process ~program:p ~args:arguments] will create a {!t}
    that runs [p] with [args] as arguments when run.
*)


val run_process :
  'a t
  -> ?input:UnixLabels.file_descr
  -> ?output:UnixLabels.file_descr
  -> ?error:UnixLabels.file_descr
  -> unit
  -> 'a
(** [run_process p ()] will run [p] and return its result

    Optionally the labelled arguments [~input], [~output], and [~error]
    can be provided. They default to {!UnixLabels.stdin},
    {!UnixLabels.stdout}, and {!UnixLabels.stderr} by default.
 *)

val run_process' : 'a t -> unit
(** [run_process' p] will run the process with the default
    values described in {!run_process} and will ignore the
    result.
*)


val pipe : 'a t ->  'b t -> ('a * 'b) t
(** [pipe p1 p2] will pipe the output of [p1] to the input
    of [p2]. The result is a tuple with the outputs of
    [p1] and [p2].
*)

val ( ||> ) : 'a t -> 'b t -> 'b t
(** Infix version of {!pipe}. *)

val ( <|| ) : 'b t -> 'a t -> 'b t
(** Flipped version of {!(||>)}. *)

val ( <<< ) : 'a t -> string -> 'a t
(** [p <<< str] will make a {!t} that runs [str] as an
    input to [p].
*)
