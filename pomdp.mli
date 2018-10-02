type prob = float

type prob_index = int

type coordinates = prob_index list

val maximize
  : ?trace       : bool
  -> (prob list) list
  -> init        : coordinates
  -> max         : (coordinates -> coordinates -> coordinates)
  -> epsillon    : float
  -> coordinates
