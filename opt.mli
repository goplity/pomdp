type 'a t = 'a option

val update : 'a t -> f:('a -> 'b) -> default:'b -> 'b t

val get_assert : 'a t -> 'a
