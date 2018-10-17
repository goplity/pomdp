type num =
  [`Int of int | `Float of float]

type t

val create : unit -> t

val count : t -> num -> unit

val report : t -> unit
