type prob = float

type prob_index = int

type coordinates = prob_index list

val maximize :
  (prob list) list 
  -> init        :'a
  -> get         : (coordinates -> 'a)
  -> compare     : 'a Order.compare
  -> coefficient : float
  -> 'a
