type 'a state =
  { coords      : Space.coordinates
  ; agents      : ('a Agent.t) list
  ; iterations  : int
  ; converged   : bool
  }

val maximize
  : ?trace               : bool
  -> spaces              : 'a Space.t list
  -> space_val_to_string : ('a -> string)
  -> init_coords_max     : Space.coordinates
  -> coefficient         : float
  -> epsilon             : float
  -> 'a state
