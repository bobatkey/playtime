open Selections.Make (Symbolic)
open Symbolic

let int_pair (u1, u2) =
  pair (const_i u1) (const_i u2)

let switch_pair ~kind:(desc1, desc2) (v1, v2) cases =
  switch desc1 v1 (fun x -> switch desc2 v2 (fun y -> cases (x, y)))

module PrisonersDilemma = struct

  type move = Cooperate | Defect
  let move = ["Cooperate", Cooperate; "Defect", Defect]

  let outcome (p1, p2) = int_pair @@ match p1, p2 with
    | Cooperate, Cooperate -> (-2, -2)
    | Cooperate, Defect    -> (-10, 0)
    | Defect,    Cooperate -> (0, -10)
    | Defect,    Defect    -> (-5, -5)

  let prisoners_dilemma strategy1 strategy2 =
    let* strategy1 = get_fst @ select (mixed move) strategy1
    and* strategy2 = get_snd @ select (mixed move) strategy2
    in
    let* move1 = sample strategy1
    and* move2 = sample strategy2
    in
    return (switch_pair ~kind:(move, move) (move1, move2) outcome)

  let pd_nash_equilbrium () =
    let open Query in
    let@ strategy1 = mixed move in
    let@ strategy2 = mixed move in
    where (equilibrium (prisoners_dilemma strategy1 strategy2))

  let pd_other_nash_equilbrium () =
    let open Query in
    let@ strategy1 = mixed move in
    let@ strategy2 = mixed move in
    wheres [
        equilibrium (prisoners_dilemma strategy1 strategy2);
        disj
          (gt (Dist.p_true (Dist.map (eq (choice move Cooperate)) strategy1)) (const_i 0))
          (gt (Dist.p_true (Dist.map (eq (choice move Cooperate)) strategy2)) (const_i 0))
      ]

end

module MatchingPennies  = struct

  type coin = Heads | Tails
  let coin = ["Heads", Heads; "Tails", Tails]

  let outcome (p1, p2) = int_pair @@ match p1, p2 with
    | Heads, Heads | Tails, Tails ->
       (1, -1)
    | Heads, Tails | Tails, Heads ->
       (-1, 1)

  let matching_pennies strategy1 strategy2 =
    let* strategy1 = get_fst @ select (mixed coin) strategy1
    and* strategy2 = get_snd @ select (mixed coin) strategy2
    in
    let* move1 = sample strategy1
    and* move2 = sample strategy2
    in
    return (switch_pair ~kind:(coin, coin) (move1, move2) outcome)

  let mp_nash_equilibrium () =
    let open Query in
    let@ player1_strategy = mixed coin in
    let@ player2_strategy = mixed coin in
    where (equilibrium (matching_pennies player1_strategy player2_strategy))

end

module Chicken = struct

  type move = Chicken | Dare
  let move = ["Chicken", Chicken; "Dare", Dare]

  let outcome (m1, m2) =
    int_pair
    @@ match m1, m2 with
       | Dare,    Dare    -> (0, 0)
       | Dare,    Chicken -> (7, 2)
       | Chicken, Dare    -> (2, 7)
       | Chicken, Chicken -> (6, 6)

  let chicken s1 s2 =
    let* s1 = get_fst @ select (mixed move) s1 in
    let* s2 = get_snd @ select (mixed move) s2 in
    let* m1 = sample s1 in
    let* m2 = sample s2 in
    return (switch_pair ~kind:(move, move) (m1, m2) outcome)

  let nash_equilibria () =
    let open Query in
    let@ s1 = mixed move in
    let@ s2 = mixed move in
    where (equilibrium (chicken s1 s2))

end


module BoS = struct

  let bool = [ "true", true; "false", false ]

  type preference = Bach | Stravinsky
  let preference = [ "Bach", Bach; "Stravinsky", Stravinsky ]

  let select_pair p (x, y) =
    conj
      (List.concat_map (fun (_, x1) ->
           List.map (fun (_, x2) ->
               ge (p (x,y)) (p (choice preference x1, choice preference x2))) preference)
         preference)

  let game player1_strat player2_strat coordinator =
    (* Players choose their strategies based on what happens in the
       continutation of the game. *)
    let* player1_strat = get_fst @ select select_pair player1_strat
    and* player2_strat = get_snd @ select select_pair player2_strat
    in
    (* Coordinator signals *)
    let* signal = sample coordinator
    in
    (* Strategies are resolved to choices using the signal *)
    let move1 =
      switch bool signal (function true -> fst player1_strat | false -> snd player1_strat)
    and move2 =
      switch bool signal (function true -> fst player2_strat | false -> snd player2_strat)
    in
    (* Outcome is computed *)
    return
    @@ switch_pair ~kind:(preference, preference) (move1, move2)
         begin function
           | (Bach, Bach)             -> int_pair (3, 2)
           | (Bach, Stravinsky)       -> int_pair (1, 1)
           | (Stravinsky, Bach)       -> int_pair (0, 0)
           | (Stravinsky, Stravinsky) -> int_pair (2, 3)
         end

  let solver () =
    let query =
      let open Query in
      let@ p1a = pure preference in
      let@ p1b = pure preference in
      let@ p2a = pure preference in
      let@ p2b = pure preference in
      let@ c   = mixed bool in
      wheres
        [
          not (eq p1a p1b);
          not (eq p2a p2b);
          gt (Dist.p_true (Dist.map (eq (choice bool true)) c)) (const_i 0);
          gt (Dist.p_true (Dist.map (eq (choice bool false)) c)) (const_i 0);
          equilibrium (game (p1a, p1b) (p2a, p2b) c);
        ]
    in
    solve ~query
      ~on_satisfied:(fun p1a p1b p2a p2b c -> Some ((p1a, p1b), (p2a, p2b), c))
      ~on_unsat:None

end

(*
module GiftGame = struct

  (* https://www.asc.ohio-state.edu/peck.33/Econ601/Econ601L15.pdf *)

  type proposal = Gift | NoGift
  let proposal = [ "Gift", Gift; "NoGift", NoGift ]

  type response = Accept | Reject
  let response = [ "Accept", Accept; "Reject", Reject ]

  type kind = Friendly | Enemy
  let kind = [ "Friendly", Friendly; "Enemy", Enemy ]

  let game nature proposer responder =
    let* proposer = E.fst

end
 *)
