
let rec leqNat y x =
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
  match y with
  | 0 -> True
  | S(y') -> (match x with
             | 0 -> False
             | S(x') -> geqNat x' y')
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
type nat = 0 | S of nat
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
 *   examples/mergesort.raml
 *
 * Author:
 *   Jan Hoffmann (S(S(0))015)
 *
 * Description:
 *   Mergesort with an auxiliary variable that is the logarithm of
 *   the length of the input list.
 *
 *)

;;
let rec split l =
  match l with
    | Nil()-> Pair(Nil,Nil)
    | Cons(x1,xs) ->
      (match xs with
        | Nil()-> Pair(Cons(x1,Nil),Nil)
        | Cons(x2,xs') ->
	         (match split xs' with
           | Pair(l1,l2) -> Pair(Cons(x1,l1),Cons(x2,l2))))
;;
let rec merge compare l1 l2 =
  match l1 with
    | Nil()-> l2
    | Cons(x,xs) ->
      (match l2 with
        | Nil()-> Cons(x,xs)
        | Cons(y,ys) -> ite (compare x y) Cons(x,merge compare xs l2) Cons(y,merge compare l1 ys))
;;
let rec mergesort compare log_l l =
  ifz log_l (fun unsued -> Nil)
    (fun log_l' ->
      match l with
	    | Nil()-> error
	    | Cons(x1,xs) ->
	       (match xs with
          | Nil()-> Cons(x1,Nil)
          | Cons(x2,xs') -> (match split l with
                            | Pair(l1, l2) ->
                            let l1' = mergesort compare log_l' l1 in
	                          let l2' = mergesort compare log_l' l2 in
	                          merge compare l1' l2')))
;;
let rec compare_list l1 l2 =
  match l1 with
    | Nil()-> True
    | Cons(x,xs) ->
      (match l2 with
	     | Nil()-> False
	     | Cons(y,ys) -> ite2 (eqNat x y) (compare_list xs ys) (leqNat x y))
;;
let mergesort_list = mergesort compare_list S(S(S(S(S(S(0))))))
;;

let main xs = mergesort_list xs
;;
