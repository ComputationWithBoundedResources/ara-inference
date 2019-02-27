
let rec leqNat y x =
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
  match y with
  | 0 -> True
  | S(y') -> (match x with
             | 0 -> False
             | S(x') -> geqNat x' y')
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
let main e = eval1 e
;;
