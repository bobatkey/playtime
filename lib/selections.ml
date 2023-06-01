module Make (E : Expr.S) : sig
  open E

  module Dist : sig
    type 'a t = (real expr * 'a) list
    val return : 'a -> 'a t
    val bind : 'a t -> ('a -> 'b t) -> 'b t
    val expected : real expr t -> real expr
    val everywhere : prop expr t -> prop expr
    val map : ('a -> 'b) -> 'a t -> 'b t
  end

  type 'a selectable =
    ('a -> real expr) -> 'a -> prop expr

  val pure : 't Expr.choice_desc -> 't choice expr selectable

  val mixed : 't Expr.choice_desc -> 't choice expr Dist.t selectable

  type ('r, 'a) t

  val return : 'a -> ('r, 'a) t
  val sample : 'a Dist.t -> ('r, 'a) t
  val select : 'a selectable -> 'a -> (real expr, 'a) t
  val (@)   : ('r -> 's) -> ('s, 'a) t -> ('r, 'a) t
  val ( let* ) : ('r, 'a) t -> ('a -> ('r, 'b) t) -> ('r, 'b) t
  val ( and* ) : ('r, 'a) t -> ('r, 'b) t -> ('r, 'a * 'b) t

  val eval : (_,'a) t -> 'a Dist.t

  val equilibrium : ('r, 'r) t -> prop expr
end = struct

  open E

  module Dist = struct

    type 'a t =
      (real expr * 'a) list

    let return x =
      [const 1, x]

    let bind d k =
      List.concat_map (fun (p, a) -> List.map (fun (q, b) -> (mul p q, b)) (k a)) d

    let expected d =
      sum (List.map (fun (p, q) -> mul p q) d)

    let map f d =
      List.map (fun (p, x) -> (p, f x)) d

    let everywhere d =
      conj (List.map (fun (p, t) -> disj (eq0 p) t) d)

  end

  type 'a selectable =
    ('a -> real expr) -> 'a -> prop expr

  let pure choice_desc p a =
    conj (List.map (fun (_, a') -> ge (p a) (p (choice choice_desc a'))) choice_desc)

  let mixed choice_desc p d =
    Dist.everywhere
      (Dist.map (pure choice_desc (fun x -> p (Dist.return x))) d)

  type ('r, 'a) t =
    | Return : 'a -> ('r, 'a) t
    | Select : { selectable   : 't selectable
               ; utility      : 'r -> real expr
               ; move         : 't
               ; continuation : 't -> ('r, 'a) t
               } -> ('r, 'a) t
    | Dist   : ('r, 'a) t Dist.t -> ('r, 'a) t
    (* | Choice : { desc   : 't Expr.choice_desc *)
    (*            ; choice : 't choice expr *)
    (*            ; switch : 't -> ('r, 'a) t *)
    (*            } -> ('r, 'a) t *)
  (* FIXME: 'Select' and 'Choice' are notably very similar; is there a
     common thing here? *)

  let return x = Return x

  let sample d = Dist (Dist.map return d)

  let select selectable move =
    Select { selectable; utility = Fun.id; move; continuation = return }

  let rec ( let* ) x f =
    match x with
    | Return a -> f a
    | Select x -> Select {x with continuation = (fun t -> let* x = x.continuation t in f x)}
    | Dist d   -> Dist (Dist.map (fun c -> let* x = c in f x) d)
    (* | Choice x -> Choice {x with switch = (fun t -> let* x = x.switch t in f x)} *)

  let ( and* ) x y =
    let* x = x in
    let* y = y in
    return (x, y)

  let rec (@) f = function
    | Return a ->
       Return a
    | Select x ->
       Select
         { x with
           utility = (fun r -> x.utility (f r));
           continuation = (fun a -> f @ x.continuation a)
         }
    | Dist d ->
       Dist (Dist.map (fun s -> f @ s) d)
    (* | Choice x -> *)
    (*    Choice {x with switch = (fun t -> f @ x.switch t)} *)

  let rec eval = function
    | Return a -> Dist.return a
    | Select x -> eval (x.continuation x.move)
    | Dist d   -> Dist.bind d eval
    (* | Choice _ -> failwith "eval Choice" (\*switch x.desc x.choice (fun t -> eval (x.switch t))*\) (\* FIXME: need to lift choices to distributions *\) *)

  let rec equilibrium = function
    | Return _ ->
       conj []
    | Select x ->
       conj [
           x.selectable
             (fun m' -> Dist.expected (Dist.map x.utility (eval (x.continuation m'))))
             x.move;
           equilibrium (x.continuation x.move)
         ]
    | Dist d ->
       Dist.everywhere (Dist.map equilibrium d)
    (* | Choice _ -> failwith "equilibrium Choice" *)
end
