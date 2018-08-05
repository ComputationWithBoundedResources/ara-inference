
let rec leqNat x y =
  match y with
  | 0 -> True
  | S(y') -> match x with
            | S(x') -> leqNat x' y'
            | 0 -> False
;;
let rec eqNat x y =
  match y with
  | 0 -> match x with
      | 0 -> True
      | S(x') -> False
  | S(y') -> match x with
            | S(x') -> eqNat x' y'
            | 0 -> False
;;
let rec geqNat x y =
  match x with
   | 0 -> False
   | S(x') -> match y with
              | 0 -> True
              | S(y') -> geqNat x' y'
;;
let rec ltNat x y =
  match y with
   | 0 -> False
   | S(y') -> match x with
        | 0 -> True
        | S(x') -> ltNat x' y'
;;
let rec gtNat x y =
  match x with
   | 0 -> False
   | S(x') -> match y with
             | 0 -> True
             | S(y') -> gtNat x' y'


;;
let ifz n th el = match n with
   | 0 -> th 0
   | S(x) -> el x
;;
let ite b th el = match b with
   | True()-> th
   | False()-> el
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
type ('a,'b,'c) triple = Triple of 'a * 'b * 'c
;;
let rec div_mod n m = match (minus n m) with
  | Pair(res,m) -> match res with
                   | 0 -> Triple (0,n,m)
                   | S(x) -> match (div_mod res m) with
                             | Triple(a,rest,unusedM) -> Triple(plus S(0) a,rest,m)
;;
let rec mult n m = match n with
   | 0 -> 0
   | S(x) -> S(plus (mult x m) m)
;;
type bool = True | False
;;
type 'a option = None | Some of 'a
;;
type 'a list = Nil | Cons of 'a * 'a list
;;
type nat = 0 | S of nat
;;
type Unit = Unit
;;
type ('a,'b) pair = Pair of 'a * 'b
;;
type var = Var of nat
;;
let eq_var vv1 vv2 = match vv1 with
  | Var(v1) -> match vv2 with
                | Var(v2) -> eqNat v1 v2
;;
type funct = Fun of nat
;;
let eq_fun ff1 ff2 = match ff1 with
  | Fun (f1) -> match ff2 with
                | Fun (f1) -> eqNat f1 f1

;;
type exp = Eadd of exp * exp
         | Emult of exp * exp
         | Ediv of exp * exp
         | Econst of nat
         | Evar of var * exp
         | Elet of var * exp * exp
         | Eapp of funct * exp
;;
let eval1 e =

  let rec eval e =
    match e with
    | Eadd(e1, e2) -> plus (eval e1) (eval e2)
    | Emult(e1, e2) -> mult (eval e1) (eval e2)
    | Ediv(e1, e2) -> (match div_mod (eval e1) (eval e2) with
                       | Triple(d,m,u) -> d)
    | Econst(n) -> n
  in eval e

;;
let eval2 e =

  let rec eval e =
    match e with
    | Eadd(e1, e2) -> plus (eval e1) (eval e2)
    | Emult(e1, e2) -> mult (eval e1) (eval e2)
    | Ediv(e1, e2) -> let n2 = eval e2
                      in ite (eqNat n2 0) (error DivByZero)
                           (match div_mod (eval e1) n2 with
                            | Triple(d,m,u) -> d)
    | Econst(n) -> n
  in eval e
;;

type Exception = VarNotFound
               | DivByZero


;;
let rec lookup eq env v =
  match env with
  | Cons(x,xs) -> (match x with
                   | Pair(u,n) -> error ite (eq u v) n (lookup eq xs v))
  | Nil()-> error VarNotFound
;;
let lookup_var = lookup eq_var
;;
let lookup_fun env v = lookup eq_fun env v


;;
let eval3 env e =

  let rec eval env e =
    match e with
    | Eadd(e1, e2) -> plus (eval env e1) (eval env e2)
    | Emult(e1, e2) -> mult (eval env e1) (eval env e2)
    | Ediv(e1, e2) -> (match div_mod (eval env e1) (eval env e2) with
                       | Triple(d,m,u) -> d)
    | Econst(n) -> n
    | Evar(v,e) -> lookup_var env v
  in eval env e

;;
let eval4 e =

  let rec eval env e =
    match e with
    (* | Eadd(e1, e2) -> plus (eval env e1) (eval env e2) *)
    (* | Emult(e1, e2) -> mult (eval env e1) (eval env e2) *)
    (* | Ediv(e1, e2) -> (match div_mod (eval env e1) (eval env e2) with
     *                    | Triple(d,m,u) -> d) *)
    (* | Econst(n) -> n *)
    | Evar(v,e) -> lookup_var env v
    | Elet(v,e1,e2) ->
      let n1 = eval env e1 in
      let env' = Cons(Pair(v,n1),env) in
      eval env' e2
  in eval Nil e

;;
let eval5 env_fun env_var e =

  let rec eval env_fun env_var e =
    match e with
    | Eadd(e1, e2) -> plus (eval env_fun env_var e1) (eval env_fun env_var e2)
    | Emult(e1, e2) -> mult (eval env_fun env_var e1) (eval env_fun env_var e2)
    | Ediv(e1, e2) -> (match div_mod (eval env_fun env_var e1) (eval env_fun env_var e2) with
                       | Triple(d,m,u) -> d)
    | Econst(n) -> n
    | Evar(vid,e) -> lookup_var env_var vid
    | Elet(v,e1,e2) ->
      let n1 = eval env_fun env_var e1 in
      let env_var' = Cons(Pair(v,n1),env_var) in
      eval env_fun env_var' e2
    | Eapp(fid,e) ->
      let f = lookup_fun env_fun fid in
      let n = eval env_fun env_var e in
      f n
  in eval env_fun env_var e
;;
let eval6 env_var e =
  let env_fun =
    Cons(Pair(Fun(S(0)), (fun x -> S(0)))
        ,Cons(Pair(Fun(S(S(0))), (fun x -> x))
            , Nil))
  in
  eval5 env_fun env_var e

(*
let eval7 e =

  let rec eval env_fun env_var e =
    match e with
    | Eadd()(e1, e2) ->
      (eval env_fun env_var e1) + (eval env_fun env_var e2)
    | Emult()(e1, e2) ->
      (eval env_fun env_var e1) * (eval env_fun env_var e2)
    | Ediv()(e1, e2) ->
      (eval env_fun env_var e1) / (eval env_fun env_var e2)
    | Econst(n) ->
      n
    | Evar()(vid,e) ->
      lookup_var env_var vid
    | Elet()(v,e1,e2) ->
      let n1 = eval env_fun env_var e1 in
      let env_var' = (v,n1)Cons(,env)_var in
      eval env_fun env_var' e2
    | Eapp()(fid,e) ->
      let (x_f,e_f) = lookup_fun env_fun fid in
      let n = eval env_fun env_var e in
      eval Nil (Cons((x_f, n),Nil)) e_f
    | Efun()(fid,x_f,e_f,e) ->
      let env_fun' = (fid,(x_f,e_f))Cons(,env)_fun in
      eval env_fun' env_var e
  in

  eval Nil Nil e
*)


;;
let main e = eval2 e
;;
