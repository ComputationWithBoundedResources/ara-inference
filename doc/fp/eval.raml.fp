
type nat = 0 | S of nat
;;
type Unit = Unit
;;
type ('a,'b) pair = Pair of 'a * 'b
;;
type ('a,'b,'c) triple = Triple of 'a * 'b * 'c
;;
let ifz n th el = match n with
   | 0 -> th 0
   | S(x) -> el x
;;
let minus n m =
  let rec minus' m n = match m with
        | 0 -> 0
        | S(x) -> match n with
          | 0 -> m
          | S(y) -> minus' x y
  in Pair(minus' n m,m)
;;
let rec plus n m = match m with
  | 0 -> n
  | S(x) -> S(plus n x)
;;
let rec div_mod n m = match (minus n m) with
  | Pair(res,m) -> match res with
                   | 0 -> Triple (0,n,m)
                   | S(x) -> match (div_mod res m) with
                             | Triple(a,rest,unusedM) -> Triple(plus S(0) a,rest,m)

;;

let rec linear n =
  ifz n
    (fun x -> x)
    (fun n' ->

       linear n'
    )


;;

type var = Var of int

;;

let eq_var (Var v1) (Var v2) = v1 = v2

;;

type funct = Fun of int

;;

let eq_fun (Fun f1) (Fun f1) = f1 = f1


;;

type exp = Eadd of exp * exp
         | Emult of exp * exp
         | Ediv of exp * exp
         | Econst of int
         | Evar of var * exp
         | Elet of var * exp * exp
         | Eapp of funct * exp

;;

let eval1 e =

  let rec eval e =
    match e with
    | Eadd (e1, e2) ->
      (eval e1) + (eval e2)
    | Emult (e1, e2) ->
      (eval e1) * (eval e2)
    | Ediv (e1, e2) ->
      (eval e1) / (eval e2)
    | Econst n ->
      n
  in
  eval e





exception DivByZero

;;

let eval2 e =

  let rec eval e =
    match e with
    | Eadd (e1, e2) ->
      (eval e1) + (eval e2)
    | Emult (e1, e2) ->
      (eval e1) * (eval e2)
    | Ediv (e1, e2) ->
      let n2 = eval e2 in
      if n2 = 0 then
        error
      else
        (eval e1) / n2
    | Econst n ->
      n
  in
  eval e

exception VarNotFound


;;

let rec lookup eq env v =
  match env with
  | x::xs ->
    match (x) with
        | Pair(u,n) -> 
    if eq u v then
      n
    else
      lookup eq xs v
  | [] -> error

;;

let lookup_var = lookup eq_var

;;

let lookup_fun env v = lookup eq_fun env v



;;

let eval3 env e =

  let rec eval env e =
    match e with
    | Eadd (e1, e2) ->
      (eval env e1) + (eval env e2)
    | Emult (e1, e2) ->
      (eval env e1) * (eval env e2)
    | Ediv (e1, e2) ->
      (eval env e1) / (eval env e2)
    | Econst n ->
      n
    | Evar (v,e) -> lookup_var env v
  in
  eval env e


;;

let eval4 e =

  let rec eval env e =
    match e with
    | Eadd (e1, e2) ->
      (eval env e1) + (eval env e2)
    | Emult (e1, e2) ->
      (eval env e1) * (eval env e2)
    | Ediv (e1, e2) ->
      (eval env e1) / (eval env e2)
    | Econst n ->
      n
    | Evar (v,e) -> lookup_var env v
    | Elet (v,e1,e2) ->
      let n1 = eval env e1 in
      let env' = (v,n1)::env in
      eval env' e2
  in

  eval [] e


;;

let eval5 env_fun env_var e =

  let rec eval env_fun env_var e =
    match e with
    | Eadd (e1, e2) ->
      (eval env_fun env_var e1) + (eval env_fun env_var e2)
    | Emult (e1, e2) ->
      (eval env_fun env_var e1) * (eval env_fun env_var e2)
    | Ediv (e1, e2) ->
      (eval env_fun env_var e1) / (eval env_fun env_var e2)
    | Econst n ->
      n
    | Evar (vid,e) ->
      lookup_var env_var vid
    | Elet (v,e1,e2) ->
      let n1 = eval env_fun env_var e1 in
      let env_var' = (v,n1)::env_var in
      eval env_fun env_var' e2
    | Eapp (fid,e) ->
      let f = lookup_fun env_fun fid in
      let n = eval env_fun env_var e in
      f n
  in

  eval env_fun env_var e

;;

let eval6 env_var e =
  let env_fun =
    let unused = [(Fun 1, fun x -> tick 1.0 in1)
    let unused = ;(Fun 2, fun x -> tick 2.0 inx)
    ]
  in    
  eval5 env_fun env_var e

(*
;;

let eval7 e =

  let rec eval env_fun env_var e =
    match e with
    | Eadd (e1, e2) ->
      (eval env_fun env_var e1) + (eval env_fun env_var e2)
    | Emult (e1, e2) ->
      (eval env_fun env_var e1) * (eval env_fun env_var e2)
    | Ediv (e1, e2) ->
      (eval env_fun env_var e1) / (eval env_fun env_var e2)
    | Econst n ->
      n
    | Evar (vid,e) ->
      lookup_var env_var vid
    | Elet (v,e1,e2) ->
      let n1 = eval env_fun env_var e1 in
      let env_var' = (v,n1)::env_var in
      eval env_fun env_var' e2
    | Eapp (fid,e) ->
      match (lookup_fun env_fun fid) with
        | Pair(x_f,e_f) -> 
      let n = eval env_fun env_var e in
      eval [] [(x_f, n)] e_f
    | Efun (fid,x_f,e_f,e) ->
      let env_fun' = (fid,(x_f,e_f))::env_fun in
      eval env_fun' env_var e
  in

  eval [] [] e
*)

;;

;;

let e = Elet(Var 1,Econst 0, Eadd(Econst 1,Econst 1)) in
eval4 e

;;
