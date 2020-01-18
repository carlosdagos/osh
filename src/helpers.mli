
val stream_map : 'a Stream.t -> ('a -> 'b) -> 'b Stream.t
(* [stream_map stream f] will map a stream of a certain
   type to another type provided a function to convert
   such types. *)

val next_while : 'a Stream.t -> ('a -> bool) -> 'a list
(* [next_while stream f] will consume elements of a stream
   based on a certain predicate. *)


val stream_chars_to_strings : char Stream.t -> string Stream.t
(* [stream_chars_to_strings stream] will map a stream of char
   to a stream of string. Every string produced is a result of
   consuming characters until the character '\n'. The last
   string could simply be the end of the stream. *)
