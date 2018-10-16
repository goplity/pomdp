type prob = float

type prob_vecs = (prob list) list

type prob_index = int

type coordinates = prob_index list

type state =
  { coordinates : coordinates
  ; prob_vecs   : prob_vecs
  ; iterations  : int
  }

val maximize
  : ?trace       : bool
  -> prob_vecs   : prob_vecs
  -> init        : coordinates
  -> max         : (coordinates -> coordinates -> [`lt | `eq | `gt])
  -> coefficient : float
  -> epsilon     : float
  -> state
