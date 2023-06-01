module PrisonersDilemma (E : Expr.S) = struct
  module S = Selections.Make (E)
  open S

  type move = Cooperate | Defect
  let move = ["Cooperate", Cooperate; "Defect", Defect]

  let mk_pair (u1, u2) =
    E.pair (E.const u1) (E.const u2)

  let outcome p1 p2 = mk_pair @@ match p1, p2 with
    | Cooperate, Cooperate -> (-2, -2)
    | Cooperate, Defect    -> (-10, 0)
    | Defect,    Cooperate -> (0, -10)
    | Defect,    Defect    -> (-5, -5)

  let game strategy1 strategy2 =
    let* strategy1 = E.fst @ select (mixed move) strategy1
    and* strategy2 = E.snd @ select (mixed move) strategy2
    in
    let* move1 = sample strategy1
    and* move2 = sample strategy2
    in
    return (E.switch move move1
            @@ fun m1 ->
               E.switch move move2
               @@ fun m2 ->
                    outcome m1 m2)
end

module MatchingPennies (E: Expr.S) = struct
  module S = Selections.Make (E)
  open S

  type move = Heads | Tails
  let move = ["Heads", Heads; "Tails", Tails]

  let mk_pair (u1, u2) =
    E.pair (E.const u1) (E.const u2)

  let outcome p1 p2 = mk_pair @@ match p1, p2 with
    | Heads, Heads | Tails, Tails ->
       (1, -1)
    | Heads, Tails | Tails, Heads ->
       (-1, 1)

  let game strategy1 strategy2 =
    let* strategy1 = E.fst @ select (mixed move) strategy1
    and* strategy2 = E.snd @ select (mixed move) strategy2
    in
    let* move1 = sample strategy1
    and* move2 = sample strategy2
    in
    return (E.switch move move1
            @@ fun m1 ->
               E.switch move move2
               @@ fun m2 ->
                    outcome m1 m2)
end

let pd_equib =
  let module G = PrisonersDilemma (Symbolic) in
  let open Symbolic in
  let p = v "p" in
  let q = v "q" in
  let strategy1 =
    [p, choice G.move G.Cooperate; sub (const 1) p, choice G.move G.Defect] in
  let strategy2 =
    [q, choice G.move G.Cooperate; sub (const 1) q, choice G.move G.Defect]
  in
  G.S.equilibrium (G.game strategy1 strategy2)

let mp_equib () =
  let module G = MatchingPennies (Symbolic) in
  let open Symbolic in
  let pH = v "pH" and pT = v "pT" in
  let qH = v "qH" and qT = v "qT" in
  let strategy1 =
    [pH, choice G.move G.Heads; pT, choice G.move G.Tails] in
  let strategy2 =
    [qH, choice G.move G.Heads; qT, choice G.move G.Tails]
  in
  Format.printf "(declare-const pH Real)\n";
  Format.printf "(declare-const pT Real)\n";
  Format.printf "(assert (and (>= pH 0) (>= pT 0) (= 1 (+ pH pT))))\n";

  Format.printf "(declare-const qH Real)\n";
  Format.printf "(declare-const qT Real)\n";
  Format.printf "(assert (and (>= qH 0) (>= qT 0) (= 1 (+ qH qT))))\n";

  Format.printf "@[<hov2>(assert@ %a)@]\n"
    Symbolic.pp_smtlib
    (G.S.equilibrium (G.game strategy1 strategy2));
  Format.printf "(check-sat)\n";
  Format.printf "(get-model)\n"

(* For a mixed Nash equilibrium problem:

   - Assume two players
   -

 *)

let () = ()
