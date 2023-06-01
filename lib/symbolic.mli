include Expr.S

val pp_smtlib : Format.formatter -> 'a expr -> unit

val v : string -> real expr

val sub : real expr -> real expr -> real expr
