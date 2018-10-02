type t = Lt | Eq | Gt

type 'a compare = ('a -> 'a -> t)

val compare : 'a compare
