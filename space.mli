type 'a t

type index = int

type coordinates = index list

type gen_spec =
  { rows  : int
  ; cols  : int
  ; order : [`inc | `dec | `ran]
  }

val from : [`read | `gen of gen_spec] -> int t

val init_prob_of_indices_per_dim : 'a t -> float list list

val get : 'a t -> coordinates -> 'a

val size : 'a t -> int
(** n_rows * n_cols *)

val iter : 'a t -> f:('a -> unit) -> unit

val print : 'a t -> to_string:('a -> string) -> indent:string -> unit
