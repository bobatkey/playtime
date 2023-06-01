type 't choice_desc = (string * 't) list

module type S = sig
  type 'a expr

  type prop
  type real
  type 'a choice

  val conj : prop expr list -> prop expr
  val disj : prop expr -> prop expr -> prop expr

  val const : int -> real expr
  val eq0   : real expr -> prop expr
  val ge    : real expr -> real expr -> prop expr
  val mul   : real expr -> real expr -> real expr
  val sum   : real expr list -> real expr

  val choice : 'a choice_desc -> 'a -> 'a choice expr
  val switch : 'a choice_desc -> 'a choice expr -> ('a -> 'b expr) -> 'b expr

  val pair : 'a expr -> 'b expr -> ('a * 'b) expr
  val fst  : ('a * 'b) expr -> 'a expr
  val snd  : ('a * 'b) expr -> 'b expr
end
