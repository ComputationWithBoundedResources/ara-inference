
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

type 'a option = None | Some of 'a
;;
;;

type ('a,'b) pair = Pair of 'a * 'b
;;
;;

type 'a list = Nil | Cons of 'a * 'a list
;;
;;

type Unit = Unit

(* * * * * * * * * * *
 * Resource Aware ML *
 * * * * * * * * * * *
 *
 * * *  Use Cases  * *
 *
 * File:
 *   examples/quicksort_mutual.raml
 *
 * Author:
 *   Ankush Das, Jan Hoffmann (2015)
 *
 * Description:
 *   A mutually recursive implementation of the quick sort algorithm as
 *   given in the article 'Depent types for program termination
 *   verification' by Hongwei Xi(Logic in Computer Science, 2001).
 *
 *   My experiments indicate that RAML computes a tight bound on the
 *   number of evaluation steps that are used by the function qs.
 *
 *   It is interesting that the inputs that produce the worst-case behavior
 *   are not reversely sorted lists [-1,-2,-3,-4,-5,-6] as usual.  The
 *   reason is that it makes a difference for functional implementations of
 *   quick sort whether the first or second recursive call receives a
 *   longer input list.  Because of the function app, the worst-case
 *   behavior emerges if the first argument of app is as long as
 *   possible.  As a result, worst-case inputs have the form
 *   [-1,-3,-5,-6,-4,-2].
 *)


;;
;;

let rec app l1 l2 =
	match l1 with
	| Nil -> l2
	| Cons(x,xs) -> ( Cons(x,app) xs l2)
;;
;;

let rec part y l r xs =
	match xs with
	| Nil -> ( app (qs l) (Cons(y,(qs r))))
	| Cons(x,xs') ->
		(
		if ((x) < (y)) then part y (Cons(x,l)) r xs'
		else part y l (Cons(x,r)) xs')
  and
    qs l =
	match l with
	| Nil -> Nil
	| Cons(x,xs) -> ( part x Nil Nil xs)

qs [-1;-3;-5;-7;-9;-10;-8;-6;-4;-2]
;;
