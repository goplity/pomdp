type prob = float

type prob_index = int

type coordinates = prob_index list

val maximize
  : ?trace       : bool
  -> (prob list) list
  -> init        :'a
  -> get         : (coordinates -> 'a)
  -> compare     : 'a Order.compare
  -> coefficient : float
  -> 'a
