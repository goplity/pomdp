type t = Lt | Eq | Gt

type 'a compare = ('a -> 'a -> t)

let compare x y =
  match Stdlib.compare x y with
  | n when n < 0 -> Lt
  | n when n > 0 -> Gt
  | _            -> Eq
