type prob = float

type prob_vecs = (prob list) list

type prob_index = int

type coordinates = prob_index list

type state =
  { coordinates : coordinates
  ; prob_vecs   : prob_vecs
  ; iterations  : int
  ; converged   : bool
  }

val maximize
  : ?trace       : bool
  -> prob_vecs   : prob_vecs
  -> init        : coordinates
  -> cmp         : (coordinates -> coordinates -> [`LT | `EQ | `GT])
  -> coefficient : float
  -> epsilon     : float
  -> state
