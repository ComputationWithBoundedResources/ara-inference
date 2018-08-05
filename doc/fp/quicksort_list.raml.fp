
let rec leqNat x y =
  match y with
  | 0 -> True
  | S(y') -> (match x with
            | S(x') -> leqNat x' y'
            | 0 -> False)
;;
let rec eqNat x y =
  match y with
  | 0 -> (match x with
      | 0 -> True
      | S(x') -> False)
  | S(y') -> (match x with
            | S(x') -> eqNat x' y'
            | 0 -> False)
;;
let rec geqNat x y =
  match x with
   | 0 -> False
   | S(x') -> (match y with
              | 0 -> True
              | S(y') -> geqNat x' y')
;;
let rec ltNat x y =
  match y with
   | 0 -> False
   | S(y') -> (match x with
        | 0 -> True
        | S(x') -> ltNat x' y')
;;
let rec gtNat x y =
  match x with
   | 0 -> False
   | S(x') -> (match y with
             | 0 -> True
             | S(y') -> gtNat x' y')


;;
let ifz n th el = match n with
   | 0 -> th 0
   | S(x) -> el x
;;
let ite b th el = match b with
   | True()-> th
   | False()-> el
;;
let ite2 b th el = match b with
   | True()-> th
   | False()-> el
;;
let ite3 b th el = match b with
   | True()-> th
   | False()-> el
;;
let minus n m =
  let rec minus' m n = match m with
        | 0 -> 0
        | S(x) -> (match n with
          | 0 -> m
          | S(y) -> minus' x y)
  in Pair(minus' n m,m)
;;
let rec plus n m = match m with
  | 0 -> n
  | S(x) -> S(plus n x)
;;
type ('a,'b,'c) triple = Triple of 'a * 'b * 'c
;;
let rec div_mod n m = match (minus n m) with
  | Pair(res,m) -> (match res with
                   | 0 -> Triple (0,n,m)
                   | S(x) -> (match (div_mod res m) with
                             | Triple(a,rest,unusedM) -> Triple(plus S(0) a,rest,m)))
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

(* * * * * * * * * * *
 * Resource Aware ML *
 * * * * * * * * * * *
 *
 * * *  Use Cases  * *
 *
 * File:
 *   examples/quicksort.raml
 *
 * Author:
 *   Jan Hoffmann, Shu-Chun Weng (S(S(0))014)
 *
 * Description:
 *   Tony Hoare's quicksort for lists.
 *
 *)


;;
let rec append l1 l2 =
  match l1 with
    | Nil()-> l2
    | Cons(x,xs) -> Cons(x,(append xs l2))

;;
let rec partition f l =
  match l with
    | Nil()-> Pair(Nil,Nil)
    | Cons(x,xs) -> match partition f xs with
         | Pair(cs,bs) -> ite (f x) Pair(cs,Cons(x,bs)) Pair(Cons(x,cs),bs)

;;
let rec quicksort gt xyz = match xyz with
  | Nil()-> Nil
  | Cons(x,xs) -> (match partition (gt x) xs with
                   | Pair(ys,zs) -> append (quicksort gt ys) Cons(x,quicksort gt zs))
(* ;;
 * let not b = match b with
 *   | True -> False
 *   | False -> True
 *
 * ;;
 * let compare a b = match a with
 *   | Pair(a1,a2) -> (match b with
 *                     | Pair(b1,b2) ->
 *                        ite2 (not (eqNat a1 b1))
 *                          (ltNat a1 b1)
 *                          (leqNat a2 b2))
 * ;;
 * let quicksort_pairs = quicksort compare
 *
 * ;;
 * let main xs = quicksort_pairs xs *)


;;
let rec compare_list l1 l2 =
  match l1 with
    | Nil()-> True
    | Cons(x,xs) ->
       (match l2 with
	      | Nil()-> False
	      | Cons(y,ys) -> ite3 (eqNat x y) (compare_list xs ys) (leqNat x y))
;;
let quicksort_list = quicksort compare_list
;;
let main ll' =
  quicksort_list  ll'
;;
