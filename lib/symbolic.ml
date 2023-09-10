(* Symbolic representation of expressions for sending to constraint
   solvers. *)

(* suppress unused constructor warnings *)
type prop = Prop [@@ocaml.warning "-37"]
type real = Real [@@ocaml.warning "-37"]
type !'t choice = Choice of 't [@@ocaml.warning "-37"]

type !'a expr =
  | Conj  : prop expr list -> prop expr
  | False : prop expr
  | Disj  : prop expr * prop expr -> prop expr
  | Not   : prop expr -> prop expr
  | Cond  : prop expr * 'a expr * 'a expr -> 'a expr

  | RealVar : string -> real expr
  | Const : Q.t -> real expr
  | Eq0   : real expr -> prop expr
  | Ge    : real expr * real expr -> prop expr
  | Gt    : real expr * real expr -> prop expr
  | Mul   : real expr * real expr -> real expr
  | Sum   : real expr list -> real expr
  | Subtract : real expr * real expr -> real expr

  | ChoiceVar : string -> 't choice expr
  | Choice : 't * 't Expr.choice_desc -> 't choice expr
  | Switch : 't choice expr * 't Expr.choice_desc * ('t -> 'a expr) -> 'a expr
  | Eq     : 't choice expr * 't choice expr -> prop expr

  | Pair : 'a expr * 'b expr -> ('a * 'b) expr

let choice_idx v desc =
  let rec get_idx i = function
    | [] -> invalid_arg "Choice: value doesn't exist in description"
    | (_, v')::desc -> if v = v' then i else get_idx (i+1) desc
  in
  get_idx 0 desc

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
  | Not p ->
     Format.fprintf fmt "@[<hov2>(not@ %a)@]"
       pp_smtlib p
  | Disj (p, q) ->
     Format.fprintf fmt "@[<hov2>(or@ %a@ %a)@]"
       pp_smtlib p
       pp_smtlib q
  | Cond (b, x, y) ->
     Format.fprintf fmt "@[<hov2>(if@ %a@ %a@ %a)@]"
       pp_smtlib b
       pp_smtlib x
       pp_smtlib y

  | RealVar s ->
     Format.fprintf fmt "%s" s
  | Const q ->
     Q.pp_print fmt q
  | Eq0 expr ->
     Format.fprintf fmt "@[<hov2>(=@ %a@ 0)@]" pp_smtlib expr
  | Ge (expr1, expr2) ->
     Format.fprintf fmt "@[<hov2>(>=@ %a@ %a)@]"
       pp_smtlib expr1
       pp_smtlib expr2
  | Gt (expr1, expr2) ->
     Format.fprintf fmt "@[<hov2>(>@ %a@ %a)@]"
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

  | ChoiceVar s ->
     Format.pp_print_string fmt s
  | Choice (v, desc) ->
     let rec get_idx i = function
       | [] -> invalid_arg "Choice: value doesn't exist in description"
       | (_, v')::desc -> if v = v' then i else get_idx (i+1) desc
     in
     let idx = get_idx 0 desc in
     Format.fprintf fmt "%d" idx
  | Switch (expr, desc, cases) ->
     let rec loop idx fmt = function
       | [] -> assert false
       | [(_, x)] -> pp_smtlib fmt (cases x)
       | (_, x)::desc ->
          Format.fprintf fmt "@[<hov2>(if (= %a %d)@ %a@ %a)@]"
            pp_smtlib      expr
            idx
            pp_smtlib      (cases x)
            (loop (idx+1)) desc
     in
     loop 0 fmt desc
  | Eq (expr1, expr2) ->
     Format.fprintf fmt "@[<hov2>(=@ %a@ %a)@]"
       pp_smtlib expr1
       pp_smtlib expr2

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
    | (Disj _ | Not _ | Eq0 _ | Switch _ | Ge _ | Gt _ | Eq _ | Cond _ as x) :: xs ->
       flatten (x :: accum) xs
  in
  match conjuncts with
  | [x] -> x
  | _ -> flatten [] conjuncts

let disj x y = match x, y with
  | Conj [], _ | _, Conj [] -> Conj []
  | False, x   | x, False   -> x
  | x, y -> Disj (x, y)

let not p = Not p

let cond b x y = match b with
  | False -> y
  | Conj [] -> x
  | b -> Cond (b, x, y)

let v name = RealVar name

let const q = Const q

let const_i i = Const (Q.of_int i)

let eq0 = function
  | Const q -> if Q.equal Q.zero q then Conj [] else False
  | x       -> Eq0 x

let ge x y = match x, y with
  | Const x, Const y ->
     if x >= y then Conj [] else False
  | x, y -> Ge (x, y)

let gt x y = Gt (x, y) (* FIXME: partial evaluation *)

let mul x y = match x, y with
  | Const q, z | z, Const q ->
     if Q.equal Q.zero q then Const Q.zero
     else if Q.equal Q.one q then z
     else Mul (x, y)
  | x, y ->
     Mul (x, y)

let sum summands =
  let rec flatten accum = function
    | [] -> (match accum with [] -> Const Q.zero | [x] -> x | _ -> Sum (List.rev accum))
    | Const q :: xs when Q.equal q Q.zero ->
       flatten accum xs
    | Sum ys :: xs ->
       flatten (List.rev_append ys accum) xs
    | (Mul _ | Const _ | Switch _ | RealVar _ | Subtract _ | Cond _ as x) :: xs ->
       flatten (x :: accum) xs
  in
  flatten [] summands

let sub x y =
  Subtract (x, y)

let choice desc v = Choice (v, desc)

(* FIXME: partially evaluate known choices *)
let eq x y = match x, y with
  | Choice (x, _), Choice (y, _) ->
     if x = y then Conj [] else False
  | x, y ->
     Eq (x, y)

let switch desc expr cases = match expr with
  | Choice (v, _) -> cases v
  | expr -> Switch (expr, desc, cases)

let pair x y = Pair (x, y)

let rec get_fst : type a b. (a * b) expr -> a expr = function
  | Pair (expr, _) ->
     expr
  | Switch (expr, desc, cases) ->
     Switch (expr, desc, fun v -> get_fst (cases v))
  | Cond (b, x, y) ->
     Cond (b, get_fst x, get_fst y)

let rec get_snd : type a b. (a * b) expr -> b expr = function
  | Pair (_, expr) ->
     expr
  | Switch (expr, desc, cases) ->
     Switch (expr, desc, fun v -> get_snd (cases v))
  | Cond (b, x, y) ->
     Cond (b, get_snd x, get_snd y)





type 'a dist = (real expr * 'a) list

type 'a concrete_dist = (Q.t * 'a) list

let pp_dist pp_choice fmt dist =
  Format.fprintf fmt
    "@[<hov2>[ %a ]@]"
    (Format.pp_print_list
       ~pp_sep:(fun fmt () -> Format.fprintf fmt ";@ ")
       (fun fmt (q, v) ->
         Format.fprintf fmt "%a: %a"
           Q.pp_print q
           pp_choice v))
    dist

module ToZ3 = struct

  open Z3

  let rec of_expr : type a. context -> a expr -> Expr.expr =
    fun z3 expr ->
    match expr with
    | Conj conjuncts ->
       Boolean.mk_and z3 (List.map (of_expr z3) conjuncts)
    | False ->
       Boolean.mk_false z3
    | Disj (p, q) ->
       Boolean.mk_or z3 [of_expr z3 p; of_expr z3 q]
    | Not p ->
       Boolean.mk_not z3 (of_expr z3 p)
    | Cond (c, t, e) ->
       Boolean.mk_ite z3 (of_expr z3 c) (of_expr z3 t) (of_expr z3 e)

    | RealVar nm ->
       Arithmetic.Real.mk_const_s z3 nm
    | Const q ->
       (* FIXME: use the string interface? *)
       Arithmetic.Real.mk_numeral_nd z3 (Z.to_int @@ Q.num q) (Z.to_int @@ Q.den q)
    | Eq0 expr ->
       Boolean.mk_eq z3 (of_expr z3 expr) (Arithmetic.Real.mk_numeral_i z3 0)
    | Ge (expr1, expr2) ->
       Arithmetic.mk_ge z3 (of_expr z3 expr1) (of_expr z3 expr2)
    | Gt (expr1, expr2) ->
       Arithmetic.mk_gt z3 (of_expr z3 expr1) (of_expr z3 expr2)
    | Mul (expr1, expr2) ->
       Arithmetic.mk_mul z3 [of_expr z3 expr1; of_expr z3 expr2]
    | Sum exprs ->
       Arithmetic.mk_add z3 (List.map (of_expr z3) exprs)
    | Subtract (expr1, expr2) ->
       Arithmetic.mk_sub z3 [of_expr z3 expr1; of_expr z3 expr2]

    | ChoiceVar nm ->
       Arithmetic.Integer.mk_const_s z3 nm
    | Choice (value, desc) ->
       let idx = choice_idx value desc in
       Arithmetic.Integer.mk_numeral_i z3 idx
    | Switch (expr, desc, cases) ->
       let rec loop idx = function
         | [] -> assert false
         | [(_,x)] -> of_expr z3 (cases x)
         | (_,x)::desc ->
            Boolean.mk_ite z3
              (Boolean.mk_eq z3 (of_expr z3 expr) (Arithmetic.Integer.mk_numeral_i z3 idx))
              (of_expr z3 (cases x))
              (loop (idx+1) desc)
       in
       loop 0 desc

    | Eq (expr1, expr2) ->
       Boolean.mk_eq z3 (of_expr z3 expr1) (of_expr z3 expr2)

    | Pair (_expr1, _expr2) ->
       invalid_arg "ToZ3.of_expr: Pair"
end

module Query = struct

  type ('k, 'r) query =
    | Where : prop expr ->
              ('r, 'r) query
    | Pure  : 'a Expr.choice_desc * ('a choice expr -> ('k, 'r) query) ->
              ('a -> 'k, 'r) query
    | Mixed : 'a Expr.choice_desc * ('a choice expr dist -> ('k, 'r) query) ->
              ('a concrete_dist -> 'k, 'r) query

  let where p = Where p
  let wheres ps = Where (conj ps)
  let pure desc query = Pure (desc, query)
  let mixed desc query = Mixed (desc, query)

  let (let@) x f = x f

  let rec get_all_models z3 solver k accum =
    match Z3.Solver.check solver [] with
    | Z3.Solver.UNSATISFIABLE ->
       List.rev accum
    | Z3.Solver.UNKNOWN ->
       failwith "Symbolic.Solver.get_all_models: UNKNOWN"
    | Z3.Solver.SATISFIABLE ->
       (match Z3.Solver.get_model solver with
        | None ->
           failwith "internal error: no model available"
        | Some model ->
           let accum = k model :: accum in
           (* assert "not this model", assuming that the model only
              contains constant declarations *)
           let not_model =
             model
             |> Z3.Model.get_const_decls
             |> List.filter_map (fun decl ->
                    match Z3.Model.get_const_interp model decl with
                    | None -> None
                    | Some e -> Some (Z3.Boolean.mk_eq z3 (Z3.FuncDecl.apply decl []) e))
           in
           Z3.Solver.add solver [Z3.Boolean.(mk_not z3 (mk_and z3 not_model))];
           get_all_models z3 solver k accum)

  let execute : type k r. query:(k, r) query -> on_satisfied:k -> on_unsat:r -> r =
    fun ~query ~on_satisfied ~on_unsat ->
    let z3 = Z3.mk_context ["model","true"] in
    let solver = Z3.Solver.mk_simple_solver z3 in
    let rec instantiate : type k. (k, r) query -> int -> (Z3.Model.model -> k) -> r =
      fun query idx k ->
      match query with
      | Where prop_expr ->
         (let z3expr = ToZ3.of_expr z3 prop_expr in
          Z3.Solver.add solver [z3expr];
          match Z3.Solver.check solver [] with
          | Z3.Solver.UNSATISFIABLE ->
             on_unsat
          | Z3.Solver.UNKNOWN ->
             (* FIXME: get the error message from Z3 *)
             failwith "Symbolic.solver: UNKNOWN"
          | Z3.Solver.SATISFIABLE ->
             (match Z3.Solver.get_model solver with
              | None ->
                 failwith "No model available"
              | Some model ->
                 k model))
      | Pure (desc, query) ->
         let varnm = Printf.sprintf "p%d" idx in
         let expr  = Z3.Arithmetic.Integer.mk_const_s z3 varnm in
         let k model =
           match Z3.Model.eval model expr true with
           | None -> failwith "Unable to retrieve value from model"
           | Some expr ->
              let i = Z.to_int (Z3.Arithmetic.Integer.get_big_int expr) in
              let _, v = List.nth desc i in
              k model v
         in
         Z3.Solver.add solver
           [ Z3.Arithmetic.(mk_le z3 (Integer.mk_numeral_i z3 0) expr);
             Z3.Arithmetic.(mk_lt z3 expr (Integer.mk_numeral_i z3 (List.length desc)))
           ];
         instantiate (query (ChoiceVar varnm)) (idx+1) k
      | Mixed (desc, query) ->
         let exprs, dist =
           desc
           |> List.map
                (fun (label, value) ->
                  let varname = Printf.sprintf "p%d-%s" idx label in
                  let var     = v varname in
                  let expr    = Z3.Arithmetic.Real.mk_const_s z3 varname in
                  (expr, (var, choice desc value)))
           |> List.split
         in
         Z3.Solver.add solver
           (List.map (Z3.Arithmetic.mk_le z3 (Z3.Arithmetic.Real.mk_numeral_i z3 0)) exprs);
         Z3.Solver.add solver
           [ Z3.Boolean.mk_eq z3
               (Z3.Arithmetic.mk_add z3 exprs)
               (Z3.Arithmetic.Real.mk_numeral_i z3 1)
           ];
         let k model =
           let dist =
             List.combine exprs desc
             |> List.map
                  (fun (expr, (_, value)) ->
                    match Z3.Model.eval model expr true with
                    | None -> failwith "unable to retrieve value from model"
                    | Some expr ->
                       let q = Z3.Arithmetic.Real.get_ratio expr in
                       (q, value))
           in
           k model dist
         in
         instantiate (query dist) (idx+1) k
    in
    instantiate query 0 (fun _model -> on_satisfied)

  let execute_all : type k r. query:(k, r) query -> k -> r list =
    fun ~query k ->
    let z3 = Z3.mk_context ["model","true"] in
    let solver = Z3.Solver.mk_simple_solver z3 in
    let rec instantiate : type k. (k, r) query -> int -> (Z3.Model.model -> k) -> r list =
      fun query idx k ->
      match query with
      | Where prop_expr ->
         let z3expr = ToZ3.of_expr z3 prop_expr in
         Z3.Solver.add solver [z3expr];
         get_all_models z3 solver k []
      | Pure (desc, query) ->
         let varnm = Printf.sprintf "p%d" idx in
         let expr  = Z3.Arithmetic.Integer.mk_const_s z3 varnm in
         let k model =
           match Z3.Model.eval model expr true with
           | None -> failwith "Unable to retrieve value from model"
           | Some expr ->
              let i = Z.to_int (Z3.Arithmetic.Integer.get_big_int expr) in
              let _, v = List.nth desc i in
              k model v
         in
         Z3.Solver.add solver
           [ Z3.Arithmetic.(mk_le z3 (Integer.mk_numeral_i z3 0) expr);
             Z3.Arithmetic.(mk_lt z3 expr (Integer.mk_numeral_i z3 (List.length desc)))
           ];
         instantiate (query (ChoiceVar varnm)) (idx+1) k
      | Mixed (desc, query) ->
         let exprs, dist =
           desc
           |> List.map
                (fun (label, value) ->
                  let varname = Printf.sprintf "p%d-%s" idx label in
                  let var     = v varname in
                  let expr    = Z3.Arithmetic.Real.mk_const_s z3 varname in
                  (expr, (var, choice desc value)))
           |> List.split
         in
         Z3.Solver.add solver
           (List.map (Z3.Arithmetic.mk_le z3 (Z3.Arithmetic.Real.mk_numeral_i z3 0)) exprs);
         Z3.Solver.add solver
           [ Z3.Boolean.mk_eq z3
               (Z3.Arithmetic.mk_add z3 exprs)
               (Z3.Arithmetic.Real.mk_numeral_i z3 1)
           ];
         let k model =
           let dist =
             List.combine exprs desc
             |> List.map
                  (fun (expr, (_, value)) ->
                    match Z3.Model.eval model expr true with
                    | None -> failwith "unable to retrieve value from model"
                    | Some expr ->
                       let q = Z3.Arithmetic.Real.get_ratio expr in
                       (q, value))
           in
           k model dist
         in
         instantiate (query dist) (idx+1) k
    in
    instantiate query 0 (fun _model -> k)


  let two_player query =
    execute ~query
      ~on_satisfied:(fun x y -> Some (x,y))
      ~on_unsat:None

end

let solve = Query.execute
let solve_all = Query.execute_all
let two_player = Query.two_player
