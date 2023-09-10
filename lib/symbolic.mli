open Generalities

include Expr.S


val pp_smtlib : 'a expr pretty_printer

val not : prop expr -> prop expr

val sub : real expr -> real expr -> real expr

val eq : 't choice expr -> 't choice expr -> prop expr

val gt : real expr -> real expr -> prop expr

(*
type _ predicate

module Predicate : sig

  val prop : prop expr predicate

  val mixed : 'a Expr.choice_desc -> 'b predicate -> ('a choice expr dist -> 'b) predicate

  val pure : 'a Expr.choice_desc -> 'b predicate -> ('a choice expr -> 'b) predicate

  val (@->) : ('a -> 'b) -> 'a -> 'b

end

val to_smt : Format.formatter -> 'a predicate -> 'a -> unit
 *)

type 'a dist = (real expr * 'a) list

type 'a concrete_dist = (Q.t * 'a) list

val pp_dist : 'a pretty_printer -> 'a concrete_dist pretty_printer

module Query : sig

  type ('k, 'r) query

  val where : prop expr -> ('r, 'r) query

  val wheres : prop expr list -> ('r, 'r) query

  val pure : 't Expr.choice_desc ->
             ('t choice expr -> ('k, 'r) query) ->
             ('t -> 'k, 'r) query

  val mixed : 't Expr.choice_desc ->
              ('t choice expr dist -> ('k, 'r) query) ->
              ('t concrete_dist -> 'k, 'r) query

  val ( let@ ) : ('a -> 'b) -> 'a -> 'b

end


val solve : query:('k, 'r) Query.query -> on_satisfied:'k -> on_unsat:'r -> 'r

val solve_all : query:('k, 'r) Query.query -> 'k -> 'r list


val two_player : ('a -> 'b -> ('a * 'b) option, ('a * 'b) option) Query.query ->
                 ('a * 'b) option
