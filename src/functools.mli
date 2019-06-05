val rightProd : 'a -> 'b -> 'a * 'b
val leftProd : 'a -> 'b -> 'b * 'a
val rightProdf : 'a -> ('a -> 'b) -> 'a * 'b
val leftProdf : 'a -> ('a -> 'b) -> 'b * 'a
val fst : 'a * 'b -> 'a
val snd : 'a * 'b -> 'b
val swap : 'a * 'b -> 'b * 'a
val ( @@ ) : ('a -> 'b) -> 'a -> 'b
val ( @< ) : ('a -> 'b) -> ('c -> 'a) -> 'c -> 'b
val curry : ('a * 'b -> 'c) -> 'a -> 'b -> 'c
val uncurry : ('a -> 'b -> 'c) -> 'a * 'b -> 'c
val fix : eq:('a -> 'a -> bool) -> f:('a -> 'a) -> 'a -> 'a
val flip : ('a -> 'b -> 'c) -> 'b -> 'a -> 'c
val first : ('a -> 'b) -> 'a * 'c -> 'b * 'c
val second : ('a -> 'b) -> 'c * 'a -> 'c * 'b
val ifz : int -> (unit -> int) -> int
