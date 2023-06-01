type 'a expr = 'a

type prop = bool
type real = float
type 'a choice = 'a

let conj = List.fold_left (&&) true
let disj = (||)

let const = float_of_int
let eq0 x = x = 0.
let ge x y = x >= y
let mul = ( *. )
let sum = List.fold_left (+.) 0.

let choice _ x = x
let switch _ x k = k x

let pair x y = (x,y)
let fst = Stdlib.fst
let snd = Stdlib.snd
