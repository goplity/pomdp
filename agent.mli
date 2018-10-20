type 'a t =
  { space           : 'a Space.t
  ; pos             : int  (* Which coords position this agent handles. *)
  ; max_val         : 'a
  ; max_coord       : int
  ; dim_coord_probs : float list
  }

val make
  : pos                   : int (* Which coords position this agent handles. *)
  -> space                : 'a Space.t
  -> init_max_val         : 'a
  -> init_max_coord       : int
  -> init_dim_coord_probs : float list
  -> 'a t

val next
  : 'a t
  -> coords      : int list
  -> coefficient : float
  -> 'a t

val coord : 'a t -> int

val is_converged : 'a t -> epsilon:float -> bool

val eprint : 'a t -> val_to_string:('a -> string) -> indent:string -> unit
