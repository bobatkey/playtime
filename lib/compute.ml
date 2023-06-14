type 'a expr = 'a

type prop = bool
type real = Q.t
type 'a choice = 'a

let conj = List.fold_left (&&) true
let disj = (||)

let cond b x y = if b then x else y

let const q = q
let const_i = Q.of_int

let eq0 = Q.equal Q.zero
let ge x y = Q.geq x y
let mul = Q.mul
let sum = List.fold_left Q.add Q.zero

let choice _ x = x
let switch _ x k = k x

let pair x y = (x,y)
let get_fst = Stdlib.fst
let get_snd = Stdlib.snd
