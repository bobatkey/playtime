(* Symbolic representation of expressions for sending to constraint
   solvers. *)

(* suppress unused constructor warnings *)
type prop = Prop [@@ocaml.warning "-37"]
type real = Real [@@ocaml.warning "-37"]
type 't choice = Choice of 't [@@ocaml.warning "-37"]

type 'a expr =
  | Conj  : prop expr list -> prop expr
  | False : prop expr
  | Disj  : prop expr * prop expr -> prop expr

  | RealVar : string -> real expr
  | Const : int -> real expr
  | Eq0   : real expr -> prop expr
  | Ge    : real expr * real expr -> prop expr
  | Mul   : real expr * real expr -> real expr
  | Sum   : real expr list -> real expr
  | Subtract : real expr * real expr -> real expr

  | Choice : 't * 't Expr.choice_desc -> 't choice expr
  | Switch : 't choice expr * 't Expr.choice_desc * ('t -> 'a expr) -> 'a expr

  | Pair : 'a expr * 'b expr -> ('a * 'b) expr

(* Printing in SMTlib format *)

let rec pp_smtlib : type a. Format.formatter -> a expr -> unit =
  fun fmt expr -> match expr with
  | Conj [] ->
     Format.fprintf fmt "true"
  | Conj conjuncts ->
     Format.fprintf fmt "@[<hov2>(and@ %a)@]"
       (Format.pp_print_list ~pp_sep:Format.pp_print_space pp_smtlib) conjuncts
  | False ->
     Format.fprintf fmt "false"
  | Disj (p, q) ->
     Format.fprintf fmt "@[<hov2>(or@ %a@ %a)@]"
       pp_smtlib p
       pp_smtlib q

  | RealVar s ->
     Format.fprintf fmt "%s" s
  | Const i ->
     Format.fprintf fmt "%d" i
  | Eq0 expr ->
     Format.fprintf fmt "@[<hov2>(=@ %a@ 0)@]" pp_smtlib expr
  | Ge (expr1, expr2) ->
     Format.fprintf fmt "@[<hov2>(>=@ %a@ %a)@]"
       pp_smtlib expr1
       pp_smtlib expr2
  | Mul (expr1, expr2) ->
     Format.fprintf fmt "@[<hov2>(*@ %a@ %a)@]"
       pp_smtlib expr1
       pp_smtlib expr2
  | Sum exprs ->
     Format.fprintf fmt "@[<hov2>(+@ %a)@]"
       (Format.pp_print_list ~pp_sep:Format.pp_print_space pp_smtlib) exprs
  | Subtract (expr1, expr2) ->
     Format.fprintf fmt "@[<hov2>(-@ %a@ %a)@]"
       pp_smtlib expr1
       pp_smtlib expr2

  | Choice (_v, _desc) ->
     Format.fprintf fmt "FIXME: choice expression"
  | Switch (_expr, _desc, _cases) ->
     Format.fprintf fmt "FIXME: cases"

  | Pair (x, y) ->
     Format.fprintf fmt "@[<hov2>(pair@ %a@ %a)@]"
       pp_smtlib x
       pp_smtlib y

(* Constructors for generating a reduced representation *)

let conj conjuncts =
  let rec flatten accum = function
    | [] -> Conj (List.rev accum)
    | False :: _ -> False
    | Conj ys :: xs -> flatten (List.rev_append ys accum) xs
    | (Disj _ | Eq0 _ | Switch _ | Ge _ as x) :: xs -> flatten (x :: accum) xs
  in
  match conjuncts with
  | [x] -> x
  | _ -> flatten [] conjuncts

let disj x y = match x, y with
  | Conj [], _ | _, Conj [] -> Conj []
  | False, x   | x, False   -> x
  | x, y -> Disj (x, y)

let v name = RealVar name

let const i = Const i

let eq0 = function
  | Const 0 -> Conj []
  | Const _ -> False
  | x       -> Eq0 x

let ge x y = match x, y with
  | Const x, Const y ->
     if x >= y then Conj [] else False
  | x, y -> Ge (x, y)

let mul x y = match x, y with
  | Const 0, _ | _, Const 0 -> Const 0
  | Const 1, x | x, Const 1 -> x
  | x, y -> Mul (x, y)

let sum summands =
  let rec flatten accum = function
    | [] -> (match accum with [] -> Const 0 | _ -> Sum (List.rev accum))
    | Const 0 :: xs ->
       flatten accum xs
    | Sum ys :: xs ->
       flatten (List.rev_append ys accum) xs
    | (Mul _ | Const _ | Switch _ | RealVar _ | Subtract _ as x) :: xs ->
       flatten (x :: accum) xs
  in
  flatten [] summands

let sub x y =
  Subtract (x, y)

let choice desc v = Choice (v, desc)

let switch desc expr cases = match expr with
  | Choice (v, _) -> cases v
  | expr -> Switch (expr, desc, cases)

let pair x y = Pair (x, y)

let rec fst : type a b. (a * b) expr -> a expr = function
  | Pair (expr, _) ->
     expr
  | Switch (expr, desc, cases) ->
     Switch (expr, desc, fun v -> fst (cases v))

let rec snd : type a b. (a * b) expr -> b expr = function
  | Pair (_, expr) -> expr
  | Switch (expr, desc, cases) ->
     Switch (expr, desc, fun v -> snd (cases v))

(*
type 'a dist = (real expr * 'a) list

type 'a predicate =
  | End   : prop expr predicate
  | Mixed : 'a Expr.choice_desc * 'b predicate -> ('a choice expr dist -> 'b) predicate
  | Pure  : 'a Expr.choice_desc * 'b predicate -> ('a choice expr -> 'b) predicate

 *)
