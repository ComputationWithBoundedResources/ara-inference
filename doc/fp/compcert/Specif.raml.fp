
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

type ('a,'b) pair = Pair of 'a * 'b
;;
;;

type 'a list = Nil | Cons of 'a * 'a list
;;
;;

type nat = 0 | S of nat
;;
;;

type Unit = Unit
;;
;;

type 'a coq_sig =
  'a
  Pair(* singleton inductive, whose constructor was exist *)
;;
;;

type 'a sig2 =
  'a
  Pair(* singleton inductive, whose constructor was exist2 *)
;;
;;

type ('a, 'p) sigT =
| Coq_existT of 'a * 'p

(** val projT1 : Pair('a1, 'a2) sigT -> 'a1 **)
;;
;;

let projT1 = function(x) ->
match x with
| Coq_existT(a, p) -> a

(** val projT2 : Pair('a1, 'a2) sigT -> 'a2 **)
;;
;;

let projT2 = function(x) ->
match x with
| Coq_existT(x0, h) -> h
;;
;;

type 'a coq_Exc = 'a option

(** val value : 'a1 -> 'a1 option **)
;;
;;

let value x =
  Some x

(** val error : 'a1 option **)
;;
;;

let error =
  None

 ()
;;