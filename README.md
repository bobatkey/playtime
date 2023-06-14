# Playtime: Representing Strategic Games and Solution Concepts as Monadic Programs

Playtime is an OCaml library for describing strategic games and their solution concepts in a monadic Embedded Domain Specific Language. Games can then be queried to answer questions about their outcomes and equilibrium points.

**Warning: work in progress; this document and the accompanying library are not finished.**

## Representing Games

The classic [Prisoner's Dilemma game](https://en.wikipedia.org/wiki/Prisoner's_dilemma) can be represented as an OCaml program as follows. First, we define the possible moves and enumerate them with human-readable labels:

```ocaml
type move = Cooperate | Defect
let move = ["Cooperate", Cooperate; "Defect", Defect]
```

Next we define the payoffs for each pair of moves by the two players:

```ocaml
let outcome (p1, p2) = int_pair @@ match p1, p2 with
  | Cooperate, Cooperate -> (-2, -2)
  | Cooperate, Defect    -> (-10, 0)
  | Defect,    Cooperate -> (0, -10)
  | Defect,    Defect    -> (-5, -5)
```

The `int_pair` reflects the OCaml integers into the embedded DSL.

Finally, we represent the game as a monadic program. We use `let*` and `and*` as the monadic binding constructs:

```ocaml
let prisoners_dilemma strategy1 strategy2 =
  let* strategy1 = get_fst @ select (mixed move) strategy1
  and* strategy2 = get_snd @ select (mixed move) strategy2
  in
  let* move1 = sample strategy1
  and* move2 = sample strategy2
  in
  return (switch_pair ~kind:(move, move) (move1, move2) outcome)
```

This definition breaks down as follows:

1. The `prisoners_dilemma` game is parameterised by the strategies for `player1` and `player2`. We are interested in so-called “mixed” strategies where each players' strategy is a probability distribution over their possible moves.
2. The next two lines define the selection criteria that each player uses to select their moves. The first player selects a mixed strategy based on the first component of the payoff pair. The second player selects a mixed strategy based on the *second* component of the payoff pair.
3. The following two lines sample the two strategies to reveal which moves are actually taken by the two players.
4. The final line returns the final payoffs using the `outcome` function defined previously. The `switch_pair` function lifts the `match` represented by the `outcome` function into the embedded DSL.

## Querying Games

Given our representation of the Prisoner's Dilemma, we can now query it to discover its properties. This query asks for a Nash equilibrium for the game:

```ocaml
let pd_nash_equilibrium () =
  let open Query in
  let@ strategy1 = mixed move in
  let@ strategy2 = mixed move in
  where (equilibrium (prisoners_dilemma strategy1 strategy2))
```

The query states that we are looking for two mixed strategies, `strategy1` and `strategy2`, that are in equilibrium for the prisoner's dilemma gamme.

Solving this query yields a Nash equilibrium for this game. This uses [Z3](FIXME) to do the actual solving:

```ocaml
utop # solve
         ~query:(pd_nash_equilibrium ())
		 ~on_satisfied:(fun s1 s2 -> Some (s1, s2))
		 ~on_unsat:None;;
= Some ([(0, Cooperate); (1, Defect)], [(0, Cooperate); (1, Defect)])
```

So the strategy profile where both players defect with probability `1` is a Nash equilibrium.

We can also ask if there are any Nash equilibria where the probability of either player `Cooperating` is greater than `0`:

```ocaml
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
```

This query has no solutions:

```ocaml
utop # solve
         ~query:(pd_other_nash_equilbrium ())
		 ~on_satisfied:(fun s1 s2 -> Some (s1, s2))
		 ~on_unsat:None;;
= None
```

## Example with Mixed Equilibria

The following definitions describe the [Matching Pennies](FIXME) game. It is similar to Prisoner's Dilemma except that the payoffs are different, and the moves have been renamed:

```ocaml
type coin = Heads | Tails
let coin = ["Heads", Heads; "Tails", Tails]

let outcome (p1, p2) = int_pair @@ match p1, p2 with
  | Heads, Heads | Tails, Tails ->
	 (1, -1)
  | Heads, Tails | Tails, Heads ->
	 (-1, 1)

let game strategy1 strategy2 =
  let* strategy1 = get_fst @ select (mixed coin) strategy1
  and* strategy2 = get_snd @ select (mixed coin) strategy2
  in
  let* move1 = sample strategy1
  and* move2 = sample strategy2
  in
  return (switch_pair ~kind:(coin, coin) (move1, move2) outcome)
```

A similar query to before asks for a Nash equilibrium:

```ocaml
let mp_nash_equilibrium () =
  let open Query in
  let@ player1_strategy = mixed coin in
  let@ player2_strategy = mixed coin in
  where (equilibrium (matching_pennies player1_strategy player2_strategy))
```

This time, there is an equilibrium where both players play heads and tails with probability `1/2`:

```ocaml
let mp_nash_equilibrium () =
  let open Query in
  let@ player1_strategy = mixed coin in
  let@ player2_strategy = mixed coin in
  where (equilibrium (matching_pennies player1_strategy player2_strategy))
```

These examples are in the file [lib/example.ml].

## Correlated and Bayesian Equilibria

TBD

## Sequential Games

TBD

## How does it work?

The key data structure is a monad combining operations for probabilistic choice with a special operation for making specific choices whilst leaving open alternative choices:

```ocaml
type ('r, 'a) t =
  | Return : 'a -> ('r, 'a) t
  | Select : { selectable   : 't selectable
			 ; utility      : 'r -> real expr
			 ; move         : 't
			 ; continuation : 't -> ('r, 'a) t
			 } -> ('r, 'a) t
  | Dist   : ('r, 'a) t Dist.t -> ('r, 'a) t
```

1. The `Return` constructor marks the outcome of a game.
2. The `Select` constructor works for any “selectable” type (one that has a function of the form `('t -> real expr) -> t -> bool expr`). It accepts a proposed `move`, but the continuation allows for must work for any potential `move`. When evaluating to learn about equilibria, the `utility` function is used to determine which moves are optimal at this point. This constructor is used to implement the `select` function used in the definitions of games above.
3. The `Dist` constructor allows the remainder of the game to be selected from a finite probability distribution. This will also affect how each player sees the payoffs from the game.

This monad is defined in [lib/selections.ml].

## Related work

- [Open Game Engine](https://github.com/CyberCat-Institute/open-game-engine)
- [Haskell Diegetic Games](https://github.com/mattecapu/haskell-diegetic-games)
- [Agda Diegetic Games](https://github.com/mattecapu/agda-diegetic-games)
